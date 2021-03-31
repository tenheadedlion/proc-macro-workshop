use proc_macro2::Span;
use syn::{
    braced,
    parse::{Parse, Result},
    spanned::Spanned,
};
use syn::{punctuated::Punctuated, Visibility};
use syn::{Ident, Token};

#[derive(Debug)]
struct BuildError(String);

impl std::error::Error for BuildError {}

impl std::fmt::Display for BuildError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

#[derive(Debug, Default)]
struct StructDef {
    vis: Option<Visibility>,
    origin: Option<syn::Ident>,
    extended: Option<syn::Ident>,
    members: Option<Vec<Member>>,
}

#[derive(Debug, Default)]
struct Member {
    attr: Option<Attribute>,
    ident: Option<Ident>,
    ty: Option<syn::Type>,
}

#[derive(Debug)]
struct Attribute {
    path: Option<syn::Ident>,
    lhs_tok: syn::Ident,
    punct: Token![=],
    rhs_tok: syn::LitStr,
}

impl Parse for Attribute {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let lhs: syn::Ident = input.parse()?;
        let span = lhs.span();
        let p: Token![=] = input.parse()?;
        let rhs: syn::LitStr = input.parse()?;
        if lhs.to_string() != "each" {
            return Err(syn::Error::new(span, r#"expected `builder(each = "...")`"#));
        }
        Ok(Attribute {
            path: None,
            lhs_tok: lhs,
            punct: p,
            rhs_tok: rhs,
        })
    }
}

impl Parse for StructDef {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let _: Token![struct] = input.parse()?;
        let name: Ident = input.parse()?;
        let content;
        let _: syn::token::Brace = braced!(content in input);
        let fields: Punctuated<syn::Field, Token![,]> =
            content.parse_terminated(syn::Field::parse_named)?;

        let mut members = Vec::new();
        for field in fields.iter() {
            let mut member: Member = Default::default();
            member.ident = field.ident.clone();
            for attr in field.attrs.iter() {
                let mut attr2: Attribute;
                let path = Some(attr.path.get_ident().unwrap().clone());
                match attr.parse_args() {
                    Ok(attr) => {
                        attr2 = attr;
                    }
                    Err(e) => {
                        let mut span = attr.path.span();
                        span = span.join(attr.tokens.span()).unwrap_or(attr.tokens.span());
                        return Err(syn::Error::new(span, e));
                    }
                }
                attr2.path = path;
                member.attr = Some(attr2);
            }
            member.ty = Some(field.ty.clone());
            members.push(member);
        }

        Ok(StructDef {
            vis: Some(vis),
            origin: Some(name.clone()),
            extended: Some(syn::Ident::new(
                &format!("{}Builder", name),
                Span::call_site(),
            )),
            members: Some(members),
        })
    }
}

#[derive(Debug)]
struct Element {
    builder_setter: proc_macro2::TokenStream,
    builder_default: proc_macro2::TokenStream,
    builder_declare: proc_macro2::TokenStream,
    builder_to_base_assign: proc_macro2::TokenStream,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let def = syn::parse_macro_input!(input as StructDef);
    let vis = def.vis.unwrap();
    let base = def.origin.unwrap();
    let builder = def.extended.unwrap();

    let mut v = Vec::new();
    for m in def.members.as_ref().unwrap().iter() {
        let extmem;
        let base = m.ident.as_ref().unwrap();
        let builder_declare;
        let builder_to_base_assign;
        let builder_setter;
        let mut core_ty = m.ty.as_ref().unwrap().clone();
        let has_attr = m.attr.is_some();

        if has_attr {
            if let Ok(ty) = core_type_of_vec(m.ty.as_ref().unwrap()) {
                core_ty = ty;
            }
        }

        if let Ok(ty) = core_type_of_option(m.ty.as_ref().unwrap()) {
            core_ty = ty;
        }

        if let Some(ref attr) = m.attr {
            extmem = syn::Ident::new(&attr.rhs_tok.value(), Span::call_site());
            builder_setter = quote::quote_spanned! {extmem.span()=>
                fn #extmem(&mut self, arg: #core_ty) -> &mut Self {
                    self.#extmem.as_mut().unwrap().push(arg);
                    self
                }
            };
        } else {
            extmem = m.ident.as_ref().unwrap().clone();
            builder_setter = quote::quote_spanned! {extmem.span()=>
                #vis fn #extmem(&mut self, arg: #core_ty) -> &mut Self {
                    self.#extmem = Some(arg);
                    self
                }
            };
        }

        let ty = &m.ty;
        if is_option(m.ty.as_ref().unwrap()) {
            builder_declare = quote::quote! {
                #extmem: #ty,
            };
            builder_to_base_assign = quote::quote! {
                #base: self.#extmem.clone(),
            };
        } else {
            builder_declare = quote::quote! {
                #extmem: ::std::option::Option<#ty>,
            };
            builder_to_base_assign = quote::quote! {
                #base: self.#extmem.as_ref().unwrap().clone(),
            };
        }
        let builder_default;
        if is_vec(m.ty.as_ref().unwrap()) {
            builder_default = quote::quote! {
                #extmem: Some(::std::vec::Vec::new()),
            };
        } else {
            builder_default = quote::quote! {
                #extmem: None,
            };
        }

        v.push(Element {
            builder_setter: builder_setter,
            builder_default: builder_default,
            builder_declare: builder_declare,
            builder_to_base_assign: builder_to_base_assign,
        });
    }

    let member_def = v.iter().map(|e| &e.builder_declare);
    let define_builder_struct = quote::quote! {

        #vis struct #builder {
            #(#member_def)*
        }
    };

    let builder_default = v.iter().map(|e| &e.builder_default);
    let impl_base = quote::quote! {
        impl #base {
            #vis fn builder() -> #builder {
                #builder {
                    #(#builder_default)*
                }
            }
        }
    };

    let builder_to_base_assign = v.iter().map(|e| &e.builder_to_base_assign);
    let builder_builds = quote::quote! {
        #vis fn build(&self) -> ::std::result::Result<#base, ::std::boxed::Box<dyn std::error::Error>> {
            Ok(#base {
                #(#builder_to_base_assign)*
            })
        }
    };

    let builder_setter = v.iter().map(|e| &e.builder_setter);

    let impl_builder = quote::quote! {
        impl #builder {
            #(#builder_setter)*
            #builder_builds
        }
    };

    proc_macro::TokenStream::from(quote::quote! {
        #define_builder_struct
        #impl_builder
        #impl_base
    })
}

macro_rules! find_type {
    ($src:expr,$target:expr) => {{
        let mut res = false;
        if let syn::Type::Path(ref path) = $src {
            for segment in &path.path.segments {
                if segment.ident.to_string() == stringify!($target) {
                    res = true;
                    break;
                }
            }
        }
        res
    }};
}

fn is_option(ty: &syn::Type) -> bool {
    find_type!(ty, Option)
}

fn is_vec(ty: &syn::Type) -> bool {
    find_type!(ty, Vec)
}

fn core_type_of_vec(ty: &syn::Type) -> std::result::Result<syn::Type, BuildError> {
    if let syn::Type::Path(ref path) = ty {
        for segment in &path.path.segments {
            if segment.ident.to_string() == "Vec" {
                if let syn::PathArguments::AngleBracketed(ref args) = segment.arguments {
                    if args.args.len() == 1 {
                        if let syn::GenericArgument::Type(ref ty) = args.args.iter().next().unwrap()
                        {
                            return Ok(ty.clone());
                        }
                    }
                }
            }
        }
    }
    Err(BuildError(
        "Don't know what is inside Vec, or Vec doesn't exist.".to_string(),
    ))
}

fn core_type_of_option(ty: &syn::Type) -> std::result::Result<syn::Type, BuildError> {
    if let syn::Type::Path(ref path) = ty {
        for segment in &path.path.segments {
            if segment.ident.to_string() == "Option" {
                if let syn::PathArguments::AngleBracketed(ref args) = segment.arguments {
                    if args.args.len() == 1 {
                        if let syn::GenericArgument::Type(ref ty) = args.args.iter().next().unwrap()
                        {
                            return Ok(ty.clone());
                        }
                    }
                }
            }
        }
    }
    Err(BuildError(
        "Don't know what is inside Vec, or Vec doesn't exist.".to_string(),
    ))
}
