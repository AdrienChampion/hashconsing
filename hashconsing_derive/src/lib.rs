//! The derive attribute implementation for the hashconsing crate
//!
//! This automatically generates some of the boilerplate that is needed in the standard use case of hashconsing.
//!
//! There are two parts to this:
//! - the static factory which can be referenced as `<Your Type>_FACTORY`
//! - A series of constructor functions for creating each of the variants
//!
//!
//!
//! Example:
//! ```rust
//! use hashconsing::hcons;
//! use std::ops::Deref;
//!
//! // Can optionally turn off the factory or the constructor generation
//! // #[hcons(name = "Type", no_factory, no_constructors)]
//! #[hcons(name = "Type")]
//! #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
//! pub enum ActualType {
//! Named(String),
//! Arrow(Type, Type),
//! Tuple(Vec<Type>),
//! Mu(String, Type),
//! Variant(Vec<(String, Type)>),
//! }
//!
//! impl ActualType {
//!     pub fn is_named(&self) -> bool {
//!         matches!(self, Self::Named(_))
//!     }
//! }
//!
//! let named_type = Type::Named("int".to_string());
//! // Dereferences to the underlying type with access to methods
//! assert!(named_type.is_named());
//! let tuple = Type::Tuple(vec![named_type]);
//! assert!(!tuple.is_named());
//! ```

use darling::{ast::NestedMeta, util::Flag, Error, FromMeta, Result};
use proc_macro::{self, TokenStream};
use proc_macro2::Span;
use proc_macro_error::{abort_call_site, proc_macro_error};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Paren, Data, DataEnum, DeriveInput, Expr,
    ExprCall, ExprPath, FnArg, Pat, PatIdent, PatType, Path, PathArguments, PathSegment, Token,
};

#[derive(Debug, Default, FromMeta)]
#[darling(and_then = "Self::not_constructors_without_factory")]
struct MacroArgs {
    name: String,
    no_factory: Flag,
    no_constructors: Flag,
}

impl MacroArgs {
    fn not_constructors_without_factory(self) -> Result<Self> {
        if self.no_factory.is_present() && !self.no_constructors.is_present() {
            abort_call_site!(
                "unsupported flag usage: Can't implement constructors without a static factory"
            )
        };
        Ok(self)
    }
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn hcons(args: TokenStream, mut input: TokenStream) -> TokenStream {
    let parsed_input = input.clone();

    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };

    let DeriveInput {
        ident,
        vis,
        attrs,
        generics: _,
        data,
    } = parse_macro_input!(parsed_input);

    let args = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let struct_name = format_ident!("{}", args.name);
    let factory_name = format_ident!("{}_FACTORY", args.name);

    /*     let (impl_generics, ty_generics, where_clause) = generics.split_for_impl(); */

    let hash_struct = quote! {
        #(#attrs)*
        #[automatically_derived]
        #vis struct #struct_name(hashconsing::HConsed<#ident>);

        #[automatically_derived]
        impl std::ops::Deref for #struct_name {
            type Target = hashconsing::HConsed<#ident>;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };

    let hash_factory = quote! {
        hashconsing::consign! {
            let #factory_name = consign(50) for #ident ;
        }
    };

    let hash_impl = match data {
        Data::Enum(DataEnum { variants, .. }) => {
            let variant_names = variants.iter().map(|v| &v.ident);
            let (variant_field_function_args, variant_field_calling_args): (
                Vec<Punctuated<FnArg, Token![,]>>,
                Vec<Expr>,
            ) = variants
                .iter()
                .map(|v| {
                    let (arg_names, arg_types): (Vec<Expr>, Vec<FnArg>) = v
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let id = format_ident!("args{}", i);
                            (
                                {
                                    ExprPath {
                                        attrs: Vec::new(),
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: Punctuated::from_iter(vec![PathSegment {
                                                ident: id.clone(),
                                                arguments: PathArguments::None,
                                            }]),
                                        },
                                    }
                                    .into()
                                },
                                FnArg::Typed(PatType {
                                    attrs: Vec::new(),
                                    pat: Box::new(Pat::Ident(PatIdent {
                                        attrs: Vec::new(),
                                        by_ref: None,
                                        mutability: None,
                                        ident: id,
                                        subpat: None,
                                    })),
                                    colon_token: Token![:](Span::call_site()),
                                    ty: Box::new(f.ty.clone()),
                                }),
                            )
                        })
                        .unzip();

                    let calling_args = Punctuated::from_iter(arg_names);

                    let variant_name = Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: Punctuated::from_iter(vec![
                                PathSegment {
                                    ident: ident.clone(),
                                    arguments: PathArguments::None,
                                },
                                PathSegment {
                                    ident: v.ident.clone(),
                                    arguments: PathArguments::None,
                                },
                            ]),
                        },
                    });

                    let variant_expr = if calling_args.is_empty() {
                        variant_name
                    } else {
                        ExprCall {
                            attrs: Vec::new(),
                            func: Box::new(variant_name),
                            paren_token: Paren(Span::call_site()),
                            args: calling_args,
                        }
                        .into()
                    };

                    (Punctuated::from_iter(arg_types), variant_expr)
                })
                .unzip();

            quote! {
                #[automatically_derived]
                #[allow(non_snake_case)]
                impl #struct_name {
                    #(pub fn #variant_names(#variant_field_function_args) -> Self {
                        use hashconsing::HashConsign;
                        Self(#factory_name.mk(#variant_field_calling_args))
                    })*
                }
            }
        }
        _ => abort_call_site!("unsupported syntax: hashconsing expected an enum definition"),
    };

    let output = if args.no_constructors.is_present() && args.no_factory.is_present() {
        quote! {
            #hash_struct
        }
    } else if args.no_constructors.is_present() {
        quote! {
            #hash_struct

            #hash_factory
        }
    } else {
        quote! {
            #hash_struct

            #hash_factory

            #hash_impl
        }
    };

    /*     println!("{output}"); */

    input.extend::<TokenStream>(output.into());

    input
}
