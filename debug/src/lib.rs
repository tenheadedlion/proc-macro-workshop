#[warn(unused_imports)]
use proc_macro2::{Span, TokenStream};
#[warn(unused_imports)]
use syn::{parse_quote, GenericParam};
#[warn(unused_imports)]
use syn::{
    Attribute, Data, DataStruct, DeriveInput, Error, Fields, Generics, Ident, Index, LitStr,
    Member, Path, Result, Type,
};
#[warn(unused_imports)]
#[derive(Debug)]

enum Input<'a> {
    Struct(Struct<'a>),
}
#[derive(Debug)]
struct Attrs<'a> {
    path: Option<&'a Path>,
    literal: Option<LitStr>,
}
#[derive(Debug)]

struct Struct<'a> {
    pub original: &'a DeriveInput,
    //pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub fields: Vec<Field<'a>>,
}
#[derive(Debug)]
struct Field<'a> {
    pub original: &'a syn::Field,
    pub attrs: Attrs<'a>,
    pub member: Member,
    pub ty: &'a Type,
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(::std::fmt::Debug));
        }
    }
    generics
}

// to gather components and produce new code
fn impl_struct(input: Struct) -> TokenStream {
    let struct_name = input.ident;
    let generics = add_trait_bounds(input.generics.clone());
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields_body = input.fields.iter().map(|f| -> TokenStream {
        match &f.member {
            Member::Named(n) => {
                if let Some(path) = f.attrs.path {
                    if path.is_ident("debug") {
                        let fmt = f.attrs.literal.as_ref().unwrap().value();
                        return quote::quote! {
                            .field(stringify!(#n), &format_args!(#fmt, &self.#n))
                        };
                    }
                }
                quote::quote! {
                    .field(stringify!(#n), &self.#n)
                }
            }
            _ => {
                quote::quote! {}
            }
        }
    });
    let res = quote::quote! {
        impl #impl_generics ::std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                    #(#fields_body)*
                    .finish()
            }
        }
    };
    res
}

fn parse_debug_attribute<'a>(attrs: &mut Attrs<'a>, attr: &'a Attribute) -> Result<()> {

    attrs.path = Some(&attr.path);
    let meta = attr.parse_meta()?;
    if let syn::Meta::NameValue(m) = meta {
        if let syn::Lit::Str(fmt) = m.lit {
            attrs.literal = Some(fmt);
            return Ok(());
        }
    }

    Err(Error::new_spanned(attr, "Expected `debug = format`"))
}

fn get_attrs(input: &[Attribute]) -> Result<Attrs> {
    let mut attrs = Attrs {
        path: None,
        literal: None,
    };

    for attr in input {
        parse_debug_attribute(&mut attrs, attr)?;
    }

    Ok(attrs)
}

impl<'a> Field<'a> {
    fn multiple_from_syn(fields: &'a Fields, span: Span) -> Result<Vec<Self>> {
        fields
            .iter()
            .enumerate()
            .map(|(i, field)| Field::from_syn(i, field, span))
            .collect()
    }

    fn from_syn(i: usize, node: &'a syn::Field, span: Span) -> Result<Self> {
        Ok(Field {
            original: node,
            attrs: get_attrs(&node.attrs)?,
            member: node.ident.clone().map(Member::Named).unwrap_or_else(|| {
                Member::Unnamed(Index {
                    index: i as u32,
                    span,
                })
            }),
            ty: &node.ty,
        })
    }
}

impl<'a> Struct<'a> {
    // Similar to implementing Parse trait.
    // syn providers many interfaces that invoke `parse()`,
    //  for example: parse_macro_input
    // but you can do it by yourself.
    fn from_syn(node: &'a DeriveInput, data: &'a DataStruct) -> Result<Self> {
        let span = Span::call_site();
        let fields = Field::multiple_from_syn(&data.fields, span)?;
        Ok(Struct {
            original: node,
            //attrs,
            ident: node.ident.clone(),
            generics: &node.generics,
            fields,
        })
    }
}

impl<'a> Input<'a> {
    pub fn from_syn(node: &'a DeriveInput) -> Result<Self> {
        match &node.data {
            Data::Struct(data) => Struct::from_syn(node, data).map(Input::Struct),
            _ => Err(Error::new_spanned(
                node,
                "Anything else than struct is not supported",
            )),
        }
    }
}

// private start point
fn derive(node: &DeriveInput) -> Result<TokenStream> {
    let input = Input::from_syn(node)?;
    Ok(match input {
        Input::Struct(input) => impl_struct(input),
    })
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
// Can be any name, consider it to be the entrance of expansion
pub fn derive_debug(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Learnt something from **thiserror** library:
    //  it seems that parsing input into DeriveInput is more preferable.
    let input = syn::parse_macro_input!(input as DeriveInput);
    derive(&input)
        // better turn syn::Error into a compile error
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
