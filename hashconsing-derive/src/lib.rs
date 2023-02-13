use darling::FromMeta;
use proc_macro::{self, TokenStream};
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, AttributeArgs, Data, DataEnum, DeriveInput, FnArg,
    Pat, PatIdent, PatType, Token,
};

#[derive(Debug, Default, FromMeta)]
struct MacroArgs {
    name: String,
    #[darling(default)]
    impls: bool,
}

#[proc_macro_attribute]
pub fn hcons(args: TokenStream, mut input: TokenStream) -> TokenStream {
    let parsed_input = input.clone();
    let attr_args = parse_macro_input!(args as AttributeArgs);
    let DeriveInput {
        ident,
        vis,
        attrs,
        generics,
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

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let hash_struct = quote! {
        #(#attrs)*
        #vis struct #struct_name(HConsed<#ident>);

        impl std::ops::Deref for #struct_name {
            type Target = HConsed<#ident>;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };

    let hash_impl = match data {
        Data::Enum(DataEnum { variants, .. }) => {
            let variant_names = variants.iter().map(|v| &v.ident);
            let (variant_field_function_args, variant_field_calling_args): (
                Vec<Punctuated<FnArg, Token![,]>>,
                Vec<Punctuated<Ident, Token![,]>>,
            ) = variants
                .iter()
                .map(|v| {
                    let (arg_names, arg_types): (Vec<_>, Vec<_>) = v
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, f)| (format_ident!("args{}", i), &f.ty))
                        .unzip();

                    let calling_args = Punctuated::from_iter(arg_names.clone());

                    let function_args = arg_names
                        .into_iter()
                        .zip(arg_types.into_iter())
                        .map(
                            |(n, t)| {
                                FnArg::Typed(PatType {
                                    attrs: Vec::new(),
                                    pat: Box::new(Pat::Ident(PatIdent {
                                        attrs: Vec::new(),
                                        by_ref: None,
                                        mutability: None,
                                        ident: n,
                                        subpat: None,
                                    })),
                                    colon_token: Token![:](Span::call_site()),
                                    ty: Box::new(t.clone()),
                                })
                            }, /* format!("{} : {}", n, t.into_token_stream()) */
                        )
                        .collect::<Vec<_>>();

                    (Punctuated::from_iter(function_args), calling_args)
                })
                .unzip();

            quote! {
                consign! {
                    /// Factory for terms.
                    let #factory_name = consign(50) for #ident ;
                }

                impl #struct_name {
                    #(pub fn #variant_names(#variant_field_function_args) -> Self {
                        Self(#factory_name.mk(#ident :: #variant_names(#variant_field_calling_args)))
                    })*
                }
            }
        }
        _ => todo!("Doesn't yet support things that aren't enums for impl"),
    };

    let output = if args.impls {
        quote! {
            #hash_struct

            #hash_impl
        }
    } else {
        quote! {
            #hash_struct
        }
    };

    println!("{output}");

    input.extend::<TokenStream>(output.into());

    input
}
