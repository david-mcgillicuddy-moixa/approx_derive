extern crate proc_macro;

mod attrib;
mod config;

use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{parse_quote, Token, FieldPat};
use quote::{quote, format_ident};
use std::iter::FromIterator;

use attrib::{FieldMember, MemberAttribKind};
use crate::config::DeriveConfig;

enum DeriveKind {
    AbsDiffEq,
    RelativeEq,
    UlpsEq,
}

#[proc_macro_derive(AbsDiffEq, attributes(approx))]
pub fn derive_abs_diff_eq(tokens: TokenStream) -> TokenStream {
    derive_entry(tokens, DeriveKind::AbsDiffEq)
}
#[proc_macro_derive(RelativeEq, attributes(approx))]
pub fn derive_relative_eq(tokens: TokenStream) -> TokenStream {
    derive_entry(tokens, DeriveKind::RelativeEq)
}
#[proc_macro_derive(UlpsEq, attributes(approx))]
pub fn derive_ulps_eq(tokens: TokenStream) -> TokenStream {
    derive_entry(tokens, DeriveKind::UlpsEq)
}

fn derive_entry(tokens: TokenStream, kind: DeriveKind) -> TokenStream {
    match derive_approx(tokens, kind) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error().into()
    }
}

fn derive_approx(tokens: TokenStream, kind: DeriveKind) -> Result<TokenStream, syn::Error> {
    let input: syn::DeriveInput = syn::parse(tokens)?;
    let config = config::parse_attributes(&input.attrs)?;

    match &input.data {
        syn::Data::Struct(r#struct) => {
            match kind {
                DeriveKind::AbsDiffEq => derive_abs_diff_struct(&input, config, r#struct),
                DeriveKind::RelativeEq => derive_relative_struct(&input, config, r#struct),
                _ => unimplemented!()
            }
        },
        syn::Data::Enum(r#enum) => {
            match kind {
                DeriveKind::AbsDiffEq => derive_abs_diff_enum(&input, config, r#enum),
                _ => unimplemented!()
            }
        },
        syn::Data::Union(r#union) => {
            Err(syn::Error::new(r#union.union_token.span, "Unions are not supported by approx_derive"))
        }
    }
}

fn derive_abs_diff_enum(input: &syn::DeriveInput, config: DeriveConfig, obj: &syn::DataEnum) -> Result<TokenStream, syn::Error> {
    let epsilon_ty = if let Some(ref ty) = config.epsilon_ty { ty.clone() } else { get_epsilon_type_variants(obj.variants.iter()) };
    let default_epsilon = build_default_epsilon_expr(&config);
    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let compare_expr = build_comparison_body_enum(ident, obj.variants.iter());

    let out = quote! {
        #[automatically_derived]
        impl #impl_generics approx::AbsDiffEq for #ident #type_generics
            #where_clause
        {
            type Epsilon = <#epsilon_ty as approx::AbsDiffEq>::Epsilon;

            #[inline]
            fn default_epsilon() -> Self::Epsilon {
                #default_epsilon
            }

            fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
                #compare_expr
            }
        }
    };

    Ok(out.into())
}

fn derive_abs_diff_struct(input: &syn::DeriveInput, config: DeriveConfig, obj: &syn::DataStruct) -> Result<TokenStream, syn::Error> {
    let fields = &obj.fields;
    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let epsilon_ty =
        if let Some(ref ty) = config.epsilon_ty { ty.clone() } else { get_epsilon_type_fields(fields.iter()) };
    let members = get_members_from_fields(fields)?;
    let compare_expr = build_comparison_body_struct(&members, &build_abs_diff_comparison);
    let default_epsilon = build_default_epsilon_expr(&config);

    let out = quote! {
        #[automatically_derived]
        impl #impl_generics approx::AbsDiffEq for #ident #type_generics
            #where_clause
        {
            type Epsilon = <#epsilon_ty as approx::AbsDiffEq>::Epsilon;

            #[inline]
            fn default_epsilon() -> Self::Epsilon {
                #default_epsilon
            }

            fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
                #compare_expr
            }
        }
    };
    Ok(out.into())
}

fn derive_relative_struct(input: &syn::DeriveInput, config: DeriveConfig, obj: &syn::DataStruct) -> Result<TokenStream, syn::Error> {
    let fields = &obj.fields;
    let ident = &input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let epsilon_ty =
        if let Some(ref ty) = config.epsilon_ty { ty.clone() } else { get_epsilon_type_fields(fields.iter()) };
    let members = get_members_from_fields(fields)?;
    let compare_expr = build_comparison_body_struct(&members, &build_relative_comparison);

    let out = quote! {
        #[automatically_derived]
        impl #impl_generics approx::RelativeEq for #ident #type_generics
            #where_clause
        {
            #[inline]
            fn default_relative() -> Self::Epsilon {
                Self::Epsilon::default_relative()
            }

            fn abs_diff_eq(&self, other: &Self, epsilon: Self::Epsilon) -> bool {
                #compare_expr
            }
        }
    };
    Ok(out.into())
}

fn build_default_epsilon_expr(config: &DeriveConfig) -> syn::Expr {
    if let Some(val) = config.default_epsilon.clone() {
        val
    } else {
        parse_quote! { Self::Epsilon::default_epsilon() }
    }
}

fn build_exact_comparison(field_member: &FieldMember) -> syn::Expr {
    let member = &field_member.member;
    parse_quote! {
        self.#member == other.#member
    }
}

fn build_abs_diff_comparison(field_member: &FieldMember) -> syn::Expr {
    if field_member.has_attrib(MemberAttribKind::ExactEq) {
        build_exact_comparison(field_member)
    } else {
        let member = &field_member.member;
        let ty = field_member.ty();
        parse_quote! {
            approx::AbsDiffEq::abs_diff_eq(&self.#member, &other.#member, epsilon.clone() as <#ty as approx::AbsDiffEq>::Epsilon)
        }
    }
}

fn build_relative_comparison(field_member: &FieldMember) -> syn::Expr {
    if field_member.has_attrib(MemberAttribKind::ExactEq) {
        build_exact_comparison(field_member)
    } else {
        let member = &field_member.member;
        let ty = field_member.ty();
        parse_quote! {
            approx::RelativeEq::relative_eq(
                &self.#member,
                &other.#member,
                epsilon.clone() as <#ty as approx::AbsDiffEq>::Epsilon,
                max_relative.clone()
            )
        }
    }
}

fn tag_a() -> syn::Ident {
    parse_quote!(a)
}
fn tag_b() -> syn::Ident {
    parse_quote!(b)
}

// This function builds a match expression of the form (roughly):
// match (self, other) {
//     (TestEnum::Unit, TestEnum::Unit) => true,
//     (TestEnum::Unnamed(a_0, b_0), TestEnum::Unnamed(a_1, b_1)) => {
//         approx_eq(&a_0, &a_1, epsilon.clone()) &&
//           approx_eq(&b_0, &b_1, epsilon.clone())
//     }
//     (TestEnum::Named{foo: foo_a, bar: bar_a}, TestEnum::Named{foo: foo_b, bar: bar_b}) => {
//         approx_eq(&foo_a, &foo_b, epsilon.clone()) &&
//           approx_eq(&bar_a, &bar_b, epsilon.clone())
//     }
// }
fn build_comparison_body_enum(enum_ident: &syn::Ident, variants: syn::punctuated::Iter<syn::Variant>) -> syn::Expr {
    let match_arms = variants.map(|variant| -> syn::Arm {
        let (variant_a_pattern, variant_b_pattern, variant_expr) = build_variant_sections(enum_ident, variant);
        parse_quote!{( #variant_a_pattern , #variant_b_pattern ) => { #variant_expr } }
    });

    parse_quote! {
        match (self, other) {
            #(#match_arms)*
            _ => false
        }
    }
}

fn build_variant_sections(enum_ident: &syn::Ident, variant: &syn::Variant) -> (syn::Pat, syn::Pat, syn::Expr) {
    let variant_ident = &variant.ident;
    match &variant.fields {
        syn::Fields::Unit => {
            let pat_a = build_variant_unit_pat(enum_ident, variant_ident);
            let pat_b = build_variant_unit_pat(enum_ident, variant_ident);
            let expr = parse_quote!( true ); // Matching unit variants are equal
            (pat_a, pat_b, expr)
        },
        syn::Fields::Unnamed(fields_unnamed) => {
            let pat_a = build_variant_unnamed_pat(enum_ident, variant_ident, fields_unnamed, tag_a());
            let pat_b = build_variant_unnamed_pat(enum_ident, variant_ident, fields_unnamed, tag_b());
            let expr = build_variant_unnamed_expr(fields_unnamed);
            (pat_a, pat_b, expr)
        }
        syn::Fields::Named(fields_named) => {
            let pat_a = build_variant_named_pat(enum_ident, variant_ident, fields_named, tag_a());
            let pat_b = build_variant_named_pat(enum_ident, variant_ident, fields_named, tag_b());
            let expr = build_variant_named_expr(fields_named);
            (pat_a, pat_b, expr)
        }
    }
}

fn build_variant_unit_pat(enum_ident: &syn::Ident, variant_ident: &syn::Ident) -> syn::Pat {
    parse_quote! {
        #enum_ident :: #variant_ident
    }
}

fn unnamed_field_ident(variant_tag: &syn::Ident, field_ix: usize) -> syn::Ident {
    format_ident!("{}_{}", variant_tag, field_ix)
}

// Builds e.g. `Foo (a_0, a_1)`
fn build_variant_unnamed_pat(enum_ident: &syn::Ident, variant_ident: &syn::Ident, fields_unnamed: &syn::FieldsUnnamed, variant_tag: syn::Ident) -> syn::Pat {
    let field_names = fields_unnamed.unnamed.iter().enumerate().map(|(ix, field)| -> syn::Ident {
        unnamed_field_ident(&variant_tag, ix)
    });
    parse_quote! {
        #enum_ident :: #variant_ident ( #(#field_names),* )
    }
}

// Builds e.g. `abs_diff_eq(a_0, b_0, eps.clone()) && abs_diff_eq(a_1, b_1, eps.clone())`
fn build_variant_unnamed_expr(fields_unnamed: &syn::FieldsUnnamed) -> syn::Expr {
    let field_exprs = fields_unnamed.unnamed.iter().enumerate().map(|(ix, field)| -> syn::Expr {
        // FIXME: handle exact_eq, different epsilon
        let variant_a_field_name = unnamed_field_ident(&tag_a(), ix);
        let variant_b_field_name = unnamed_field_ident(&tag_b(), ix);
        let ty = &field.ty;
        parse_quote! {
            approx::AbsDiffEq::abs_diff_eq(
                & #variant_a_field_name,
                & #variant_b_field_name,
                epsilon.clone() as <#ty as approx::AbsDiffEq>::Epsilon
            )
        }
    });
    parse_quote!( #(#field_exprs)&&* )
}

fn named_field_ident(field_name: &syn::Ident, variant_tag: &syn::Ident) -> syn::Ident {
    format_ident!("{}_{}", field_name, variant_tag)
}

// Builds e.g. `Foo { bar: bar_a, baz: baz_a }`
fn build_variant_named_pat(enum_ident: &syn::Ident, variant_ident: &syn::Ident, fields_named: &syn::FieldsNamed, variant_tag: syn::Ident) -> syn::Pat {
    let fields = fields_named.named.iter().map(|field| -> syn::FieldPat {
        // FIXME: are there situations where a field from a named variant won't have an ident?
        let field_ident = &field.ident.as_ref().unwrap();
        let variant_field_name = named_field_ident(field_ident, &variant_tag);
        // Why isn't FieldPat parsable again
        syn::FieldPat {
            attrs: vec![],
            member: parse_quote!(#field_ident),
            colon_token: Some(syn::token::Colon(Span::call_site())), // FIXME: spans
            pat: Box::new(parse_quote!(#variant_field_name)),
        }
    });
    parse_quote! {
        #enum_ident :: #variant_ident { #(#fields),* }
    }
}

// Builds e.g. `abs_diff_eq(bar_a, bar_b, eps.clone()) && abs_diff_eq(baz_a, baz_b, eps.clone())`
fn build_variant_named_expr(fields_named: &syn::FieldsNamed) -> syn::Expr {
    let field_exprs = fields_named.named.iter().map(|field| -> syn::Expr {
        // FIXME: are there situations where a field from a named variant won't have an ident?
        let field_ident = field.ident.as_ref().unwrap();
        let variant_a_field_name = named_field_ident(field_ident, &tag_a());
        let variant_b_field_name = named_field_ident(field_ident, &tag_b());
        let ty = &field.ty;

        parse_quote! {
            approx::AbsDiffEq::abs_diff_eq(
                & #variant_a_field_name,
                & #variant_b_field_name,
                epsilon.clone() as <#ty as approx::AbsDiffEq>::Epsilon
            )
        }
    });
    parse_quote!{ #(#field_exprs)&&* }
}



fn build_comparison_body_struct<F>(members: &Vec<FieldMember>, expr_builder: F) -> syn::Expr
    where F: Fn(&FieldMember) -> syn::Expr
{
    let mut compare_exprs = syn::punctuated::Punctuated::<syn::Expr, Token![&&]>::new();
    for field_member in members {
        let expr = expr_builder(field_member);
        compare_exprs.push(expr);
    }
    parse_quote! { #compare_exprs }
}

fn get_field_span(field: &syn::Field) -> Span {
    if let Some(ref ident) = field.ident {
        ident.span()
    } else {
        if let syn::Type::Path(path) = &field.ty {
            path.path.segments[0].ident.span()
        } else {
            Span::call_site()
        }
    }
}

fn get_members_from_fields(fields: &syn::Fields) -> Result<Vec<FieldMember>, syn::Error> {
    let mut members = Vec::new();
    match fields {
        syn::Fields::Named(fields) => {
            for field in fields.named.iter().cloned() {
                let member = syn::Member::Named(field.ident.clone().unwrap());
                let field_members = FieldMember::new(member, field)?;
                members.push(field_members);
            }
        },
        syn::Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().cloned().enumerate() {
                let field_span = get_field_span(&field);
                let member = syn::Member::Unnamed(syn::Index { index: i as u32, span: field_span });
                let field_members = FieldMember::new(member, field)?;
                members.push(field_members);
            }
        },
        syn::Fields::Unit => {},
    }
    Ok(members)
}

// If any of the variants have any fields that require f64 then return f64 else return f32?
fn get_epsilon_type_variants(variants: syn::punctuated::Iter<syn::Variant>) -> syn::Type {
    let f32_ty: syn::Type = parse_quote! {f32};
    let f64_ty: syn::Type = parse_quote! {f64};
    let mut out_type: syn::Type = f32_ty.clone();
    for variant in variants {
        let variant_type = get_epsilon_type_fields(variant.fields.iter());
        if variant_type == f64_ty && out_type == f32_ty {
            out_type = variant_type.clone();
        }
    }
    out_type
}

// If any of the fields require f64 then return f64 else return f32?
fn get_epsilon_type_fields(fields: syn::punctuated::Iter<syn::Field>) -> syn::Type {
    let f32_ty: syn::Type = parse_quote! {f32};
    let f64_ty: syn::Type = parse_quote! {f64};
    let mut out_type: syn::Type = f32_ty.clone();
    for field in fields {
        let ty = &field.ty;
        if *ty == f64_ty && out_type == f32_ty {
            out_type = ty.clone();
        }
    }
    out_type
}

