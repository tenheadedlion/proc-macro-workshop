use proc_macro2::{Ident, Literal, Span, TokenTree};
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Field, GenericArgument, PathArguments,
    PathSegment, Type,
};
use syn::{Error, Result};

type TokenStream1 = proc_macro::TokenStream;
type TokenStream2 = proc_macro2::TokenStream;

fn data_struct(data: Data) -> Result<DataStruct> {
    match data {
        Data::Struct(d) => Ok(d),
        Data::Enum(e) => Err(Error::new(e.enum_token.span, "Expect Data::Struct")),
        Data::Union(e) => Err(Error::new(e.union_token.span, "Expect Data::Struct")),
    }
}

fn path_segment_from_type(ty: &Type) -> Result<&PathSegment> {
    if let Type::Path(tp) = ty {
        let segs = &tp.path.segments;
        if segs.len() == 1 {
            return Ok(segs.first().unwrap());
        }
    }
    Err(Error::new(ty.span(), "Fail to get PathSegment"))
}

fn is_option(ty: &Type) -> (bool, Option<&Ident>) {
    wrapped_type(ty, "Option")
}

fn wrapped_type<'a>(ty: &'a Type, wrapper: &'a str) -> (bool, Option<&'a Ident>) {
    if let Ok(sg) = path_segment_from_type(ty) {
        if sg.ident == wrapper {
            if let PathArguments::AngleBracketed(args) = &sg.arguments {
                let args = &args.args;
                if args.len() == 1 {
                    if let Some(GenericArgument::Type(ty)) = args.first() {
                        if let Ok(sg) = path_segment_from_type(ty) {
                            return (true, Some(&sg.ident));
                        }
                    }
                }
            }
        }
    }
    (false, None)
}

fn builder_attribute(field: &Field) -> Result<(bool, Option<Literal>)> {
    let attrs = &field.attrs;
    if attrs.len() == 1 {
        let attr = attrs.first().unwrap();
        let segs = &attr.path.segments;
        if segs.len() == 1 && segs[0].ident == "builder" {
            let mut tokens = attr.tokens.clone().into_iter();
            let token = tokens.next().unwrap();
            if let TokenTree::Group(grp) = token {
                let mut tokens = grp.stream().into_iter();
                let ident = tokens.next().unwrap();
                let punct = tokens.next().unwrap();
                if ident.to_string() == "each" {
                    if punct.to_string() == "=" {
                        let literal = tokens.next().unwrap();
                        if let TokenTree::Literal(literal) = literal {
                            return Ok((true, Some(literal)));
                        }
                    }
                } else {
                    return ::std::result::Result::Err(syn::Error::new(
                        grp.span(),
                        "expected `builder(each = \"...\")`".to_string(),
                    ));
                }
            }
        }
    }
    Ok((false, None))
}

fn builder_deriver(input: DeriveInput) -> Result<TokenStream2> {
    let vis = input.vis;
    let head = input.ident;
    let _generics = input.generics;
    let data = input.data;
    let builder = Ident::new(
        &(String::from(&head.to_string()) + "Builder"),
        Span::call_site(),
    );
    let data = data_struct(data).unwrap();

    let mut fields_declarer = quote! {};
    let mut fields_initializer = quote! {};
    let mut fields_setter = quote! {};
    let mut fields_copy = quote! {};
    for field in &data.fields {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let (is_option, wrapped_id) = is_option(ty);

        let mut alt_decl = quote! { ::std::option::Option<#ty> };
        let mut alt_sett = quote! { #ty };
        let s = format!("{}: Field is None ", &ident.to_string());
        let mut alt_copy = quote! { .expect(#s)  };

        let (is_repeated, each) = builder_attribute(field)?;

        if is_option {
            alt_decl = quote! { #ty };
            alt_sett = quote! { #wrapped_id };
            alt_copy = quote! {};
        }

        fields_declarer = quote! {
            #fields_declarer
            #ident: #alt_decl,
        };

        if is_repeated {
            let (_, inner) = wrapped_type(ty, "Vec");
            let e = each.unwrap();
            let each = Ident::new(
                &e.to_string()[1..e.to_string().len() - 1],
                Span::call_site(),
            );
            let inner = inner.unwrap();
            fields_setter = quote! {
                #fields_setter
                fn #each(&mut self, #each: #inner) -> &mut Self {
                    if self.#ident.is_none() {
                        self.#ident = ::std::option::Option::Some(::std::vec::Vec::new());
                    }
                    self.#ident.as_mut().unwrap().push(#each);
                    self
                }
            };
            fields_copy = quote! {
                #fields_copy
                #ident: match self.#ident {
                    None => ::std::vec::Vec::new(),
                    _ => self.#ident.clone()#alt_copy
                },
            };
        } else {
            fields_setter = quote! {
                #fields_setter
                fn #ident(&mut self, #ident: #alt_sett) -> &mut Self {
                    self.#ident = ::std::option::Option::Some(#ident);
                    self
                }
            };

            fields_copy = quote! {
                #fields_copy
                #ident: self.#ident.clone()#alt_copy,
            };
        }

        fields_initializer = quote! {
            #fields_initializer
            #ident: None,
        };
    }
    let build_func = quote! {
        pub fn build(&mut self) -> ::std::result::Result<#head, ::std::boxed::Box<dyn std::error::Error>> {
            Ok(#head {
                #fields_copy
            })
        }
    };

    Ok(quote! {
        pub struct #builder {
            #fields_declarer
        }
        impl #head {
            #vis fn builder() -> #builder
            {
                #builder {
                    #fields_initializer
                }
            }
        }
        impl #builder {
            #fields_setter
            #build_func
        }
    })
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as DeriveInput);
    builder_deriver(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
