use proc_macro2::Span;
use quote::quote;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, Arm, ExprMatch, Ident, ItemEnum, ItemFn, Pat};
use syn::{Error, Item, Result};

type TokenStream1 = proc_macro::TokenStream;
type TokenStream2 = proc_macro2::TokenStream;

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Item);
    sorted_attribute(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn sorted_attribute(input: Item) -> Result<TokenStream2> {
    match input {
        Item::Enum(e) => sorted_enum_attribute_impl(e),
        _ => Err(Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn unsorted_item<'a>(arr: &[&'a Ident]) -> Option<(&'a Ident, &'a Ident)> {
    let mut prev = arr[0];
    let mut mistack = false;
    for var in arr {
        if prev > var {
            mistack = true;
            prev = var;
            break;
        }
        prev = var;
    }
    let target = prev;
    if mistack {
        for var in arr {
            if target >= var {
                continue;
            } else {
                return Some((target, var));
            }
        }
    }
    None
}

fn unsorted_item2<'a, T>(arr: &[&'a T]) -> Option<(&'a T, &'a T)>
where
    T: std::cmp::PartialOrd,
{
    let mut prev = arr[0];
    let mut mistack = false;
    for var in arr {
        if prev > var {
            mistack = true;
            prev = var;
            break;
        }
        prev = var;
    }
    let target = prev;
    if mistack {
        for var in arr {
            if target >= var {
                continue;
            } else {
                return Some((target, var));
            }
        }
    }
    None
}

impl From<Ident> for ForgedIdent {
    fn from(i: Ident) -> Self {
        ForgedIdent {
            name: i.to_string(),
            span: i.span(),
        }
    }
}

impl From<&Ident> for ForgedIdent {
    fn from(i: &Ident) -> Self {
        ForgedIdent {
            name: i.to_string(),
            span: i.span(),
        }
    }
}

fn throw(first: ForgedIdent, second: ForgedIdent) -> Result<TokenStream2> {
    Err(Error::new(
        first.span,
        format!("{} should sort before {}", first.name, second.name),
    ))
}

fn sorted_enum_attribute_impl(input: ItemEnum) -> Result<TokenStream2> {
    if input.variants.is_empty() {
        return Ok(quote! {
            #input
        });
    }

    let arr: Vec<&Ident> = input
        .variants
        .iter()
        .map(|var: &syn::Variant| &var.ident)
        .collect();

    if let Some((first, second)) = unsorted_item(&arr) {
        return throw(first.into(), second.into());
    }

    Ok(quote! {
        #input
    })
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Item);
    check_attribute(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn check_attribute(input: Item) -> Result<TokenStream2> {
    match input {
        Item::Fn(f) => check_fn_attribute_impl(f),
        _ => Err(Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn check_fn_attribute_impl(mut input: ItemFn) -> Result<TokenStream2> {
    let mut probe = SortedProbe {
        arms: Vec::new(),
        mistake: None,
        unsupported: None,
        unordered_wildcard: None,
        expected_arms_cnt: 0,
    };
    probe.visit_item_fn_mut(&mut input);

    if let Some(e) = probe.unsupported {
        Err(Error::new(e.span, "unsupported by #[sorted]"))
    } else if let Some(e) = probe.unordered_wildcard {
        Err(Error::new(e.span, "Underscore should be the last variant"))
    } else {
        match probe.mistake {
            Some((first, second)) => throw(first, second),
            _ => Ok(quote! {
                #input
            }),
        }
    }
}
#[derive(Debug)]
struct SortedPath {
    inner: Vec<Ident>,
}
struct ForgedIdent {
    name: String,
    span: Span,
}
impl SortedPath {
    fn cmp_point(&self) -> &Ident {
        self.inner.iter().last().unwrap()
    }
    fn forged_ident(&self) -> ForgedIdent {
        let list = &self
            .inner
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>();

        ForgedIdent {
            name: list.join("::"),
            span: self.cmp_point().span(),
        }
    }
}
impl std::cmp::PartialOrd for SortedPath {
    fn lt(&self, other: &Self) -> bool {
        self.cmp_point() < other.cmp_point()
    }

    fn le(&self, other: &Self) -> bool {
        self.cmp_point() <= other.cmp_point()
    }

    fn gt(&self, other: &Self) -> bool {
        self.cmp_point() > other.cmp_point()
    }

    fn ge(&self, other: &Self) -> bool {
        self.cmp_point() >= other.cmp_point()
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.cmp_point().partial_cmp(other.cmp_point())
    }
}

impl std::cmp::PartialEq for SortedPath {
    fn eq(&self, other: &Self) -> bool {
        self.cmp_point() == other.cmp_point()
    }
}

struct SortedProbe {
    // had a bad feeling about not using references
    arms: Vec<SortedPath>,
    mistake: Option<(ForgedIdent, ForgedIdent)>,
    unsupported: Option<ForgedIdent>,
    unordered_wildcard: Option<ForgedIdent>,
    expected_arms_cnt: usize,
}

impl syn::visit_mut::VisitMut for SortedProbe {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        if i.attrs.len() == 1 {
            let attribute = i.attrs.first().unwrap();
            let segs = &attribute.path.segments;
            if segs.len() == 1 {
                let seg = segs.first().unwrap();
                if seg.ident == "sorted" {
                    self.expected_arms_cnt = i.arms.len();
                    for arm in i.arms.iter_mut() {
                        self.visit_arm_mut(arm);
                        if self.unsupported.is_some() {
                            return;
                        }
                        if let Some((_1, _2)) =
                            unsorted_item2(&self.arms.iter().collect::<Vec<&SortedPath>>())
                        {
                            self.mistake = Some((_1.forged_ident(), _2.forged_ident()));
                        } else {
                            i.attrs.clear();
                        }
                    }
                }
            }
        }
    }
    fn visit_arm_mut(&mut self, i: &mut Arm) {
        match &i.pat {
            Pat::TupleStruct(pt) => {
                if !pt.path.segments.is_empty() {
                    self.arms.push(SortedPath {
                        inner: pt
                            .path
                            .segments
                            .iter()
                            .map(|p| p.ident.clone())
                            .collect::<Vec<Ident>>(),
                    });
                }
            }
            Pat::Ident(id) => self.arms.push(SortedPath {
                inner: vec![id.ident.clone()],
            }),
            Pat::Wild(_) => {
                if self.arms.len() != self.expected_arms_cnt - 1 {
                    self.unordered_wildcard = Some(ForgedIdent {
                        name: "_".to_string(),
                        span: i.pat.span(),
                    });
                }
            }
            _ => {
                self.unsupported = Some(ForgedIdent {
                    name: "".to_string(),
                    span: i.pat.span(),
                });
            }
        }
    }
}
