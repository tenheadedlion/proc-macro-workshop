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






#[proc_macro_attribute]
pub fn bitfield(args: TokenStream1, input: TokenStream1) -> TokenStream1 {

    dbg!(args);
    dbg!(input);
    


    unimplemented!()
}
