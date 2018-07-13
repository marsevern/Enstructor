#![feature(
    rust_2018_edition,
    rustc_private,
    proc_macro,
    extern_prelude,
    iterator_flatten
)]
#![allow(dead_code)]
#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use heck::{
    CamelCase,
    SnakeCase,
};
use proc_macro::TokenStream;
use proc_macro2::{
    Span,
    TokenStream as TokenStream2,
};
use quote::{
    pounded_var_names,
    quote,
    quote_each_token,
    quote_spanned,
    ToTokens,
};
use syn::{
    punctuated::Punctuated,
    spanned::Spanned,
    synom::Synom,
    Data,
    DeriveInput,
    Token,
};

macro_rules! format_ident {
    ($format:tt, $id:expr) => {
        syn::Ident::new(&format!($format, $id).to_camel_case(), $id.span())
    };
}

macro_rules! format_ident_snake {
    ($format:tt, $id:expr) => {
        syn::Ident::new(&format!($format, $id).to_snake_case(), $id.span())
    };
}

#[proc_macro_derive(DefaultEnum, attributes(Default))]
pub fn default_enum(input: TokenStream) -> TokenStream {
    let input: proc_macro2::TokenStream = input.into();
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let variants = match ast.data {
        Data::Enum(enm) => enm.variants,
        _ => panic!("May only derive(DefaultEnum) for enums"),
    };
    let mut default_variants_matched = variants.iter().filter(|v| {
        v.attrs.iter().any(|a| {
            let path = &a.path;
            quote!(#path).to_string() == "Default"
        })
    });
    let default_variant = default_variants_matched
        .next()
        .expect("At least one variant must be marked as #[Default]");
    if default_variants_matched.count() > 0 {
        panic!("Only one variant may be marked as #[Default]");
    }
    let default_variant_ident = &default_variant.ident;
    let fields = &default_variant.fields;
    let mut generics = ast.generics;
    generics.make_where_clause();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let where_predicates = where_clause.map(|w| &w.predicates);
    use syn::Fields;
    let (args, where_clause) = match *fields {
        Fields::Unit => (quote!(), quote!()),
        Fields::Named(ref fs) => {
            let enum_fields = fs.named.iter().map(|e| {
                let (id, sep, ty) = (&e.ident, e.colon_token, &e.ty);
                quote!(#id #sep <#ty as Default> :: default())
            });
            let types = fs.named.iter().map(|e| &e.ty);
            (
                quote!({#(#enum_fields,)*}),
                quote!(where #(#types : Default,)*),
            )
        },
        Fields::Unnamed(ref fs) => {
            let enum_fields = fs.unnamed.iter().map(|e| {
                let ty = &e.ty;
                quote!(<#ty as Default> :: default())
            });
            let types = fs.unnamed.iter().map(|e| &e.ty);
            (
                quote!((#(#enum_fields,)*)),
                quote!(where #(#types : Default,)*),
            )
        },
    };
    let enum_name = &ast.ident;
    let here_default = quote!(Default);
    let out = quote!(
        impl #impl_generics #here_default for #enum_name #ty_generics
        #where_clause
        #where_predicates
        {
            fn default() -> Self {
                #enum_name::#default_variant_ident #args
            }
        }
    );
    out.into()
}

#[proc_macro_derive(DisplayEnum, attributes(DisplayText))]
pub fn display_enum(input: TokenStream) -> TokenStream {
    let input: proc_macro2::TokenStream = input.into();
    let display = display_enum_tokenstream_2(input);
    display.into()
}

fn display_enum_tokenstream_2(
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let ident = &ast.ident;
    let enum_data = ensure_enum(&ast);
    let match_arms = enum_data.variants.iter().map(|v| {
        let v_ident = &v.ident;
        let text = v_ident.to_string();
        quote_spanned!{v.span()=>
            self :: #ident :: #v_ident => #text,
        }
    });
    quote_spanned!{ast.span()=>
        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", match self {
                    #(#match_arms)*
                })
            }
        }
    }
}

#[proc_macro_attribute]
pub fn enum_inner(_: TokenStream, input: TokenStream) -> TokenStream {
    let input: proc_macro2::TokenStream = input.into();
    let mut out = TokenStream2::new();
    let item_enum = syn::parse2::<syn::ItemEnum>(input).unwrap_or_else(|_| {
        panic!("Can only use #[enum_inner] attribute on enums!")
    });
    let name = item_enum.ident.clone();
    let vis = item_enum.vis.clone();
    let generics = item_enum.generics.clone();
    let id_enum = inner_type_enum(item_enum.clone());
    let as_trait = as_type_trait(&vis, &name);
    {
        let variants_as_structs = item_enum
            .variants
            .iter()
            .map(|v| build_struct_item_from_variant(&vis, &generics, v));
        push_tokens_iter(&mut out, variants_as_structs);
        let impl_as_type = item_enum
            .variants
            .iter()
            .map(|v| as_type_impl(&name, &generics, v));
        push_tokens_iter(&mut out, impl_as_type);
    }
    let container = make_container_item(item_enum);
    container.to_tokens(&mut out);
    id_enum.to_tokens(&mut out);
    as_trait.to_tokens(&mut out);
    out.into()
}

#[proc_macro]
pub fn enum_wrapper(input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let wrapper: WrapperEnum =
        syn::parse2(input).expect("Could not parse input correctly");
    let mut out = TokenStream2::new();
    wrapper
        .as_items()
        .iter()
        .for_each(|i| i.to_tokens(&mut out));
    // panic!("{}", out);
    out.into()
}

#[proc_macro]
pub fn new_proc_macro(input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let string = format!("{}", quote!(#input));
    quote!(println!(#string);).into()
}

/// Does this show up?
#[proc_macro]
pub fn derive_display(input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let buffer = syn::buffer::TokenBuffer::new2(input);
    // let cursor = buffer.begin();
    // let (mut generics, cursor) = parse_req::<syn::Generics>(cursor);
    // let (ty, cursor) = parse_req::<syn::Type>(cursor);
    // let (where_clause, cursor) = parse_opt(cursor);
    // let (block, _) = parse_req::<syn::Block>(cursor);
    // generics.where_clause = where_clause;
    // let (impl_tys, _, where_clause) = generics.split_for_impl();
    let span = Span::call_site();
    // let span = block.span();
    let fmt_sig: Result<FnDecl, _> = syn::parse2(
        quote!(fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result),
    );
    let segments = "std,fmt,Display"
        .split(',')
        .map(|s| syn::PathSegment::from(syn::Ident::new(s, span)))
        .collect();
    let trait_ = syn::Path {
        leading_colon: None,
        segments,
    };
    // let cursor = buffer.begin();
    let dt: DeriveTrait = Synom::parse(buffer.begin()).unwrap().0;
    // let trait_ =
    dt.build(fmt_sig.into_iter().map(Into::into).collect(), trait_)
        .into_token_stream()
        .into()
    // let trait_ = parse_to_trait_impl(trait_, fmt_sig, cursor);
    // quote!(
    //     #trait_
    //     // impl #impl_tys std::fmt::Display for #ty
    //     // #where_clause
    //     // {
    //     //     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    //     //     #block
    //     // }
    // ).into()
}

/// pub enum Inner {
/// 	struct StructLike {
/// 		named_field: str,
/// 		other_field: u8
/// 	}
/// 	TupleLike(u16, String),
/// 	UnitLike,
/// 	enum InnerEnum {
/// 		UnitVar,
/// 		TupleVar(u32),
/// 		StructVar(field: u64)
/// 	}
/// }
///

#[derive(Clone)]
struct WrapperEnum {
    attrs:       Vec<syn::Attribute>,
    vis:         syn::Visibility,
    enum_token:  Token![enum],
    ident:       syn::Ident,
    generics:    syn::Generics,
    brace_token: syn::token::Brace,
    inner_types: Vec<WrapperInner>,
}

impl WrapperEnum {
    fn as_items(&self) -> Vec<syn::Item> {
        self.inner_types.iter().cloned().map(Into::into).collect()
    }
}
impl Synom for WrapperEnum {
    named!{
        parse -> Self,
        do_parse!(
            attrs: many0!(syn::Attribute::parse_outer) >>
            vis: syn!(syn::Visibility) >>
            enum_token: keyword!(enum) >>
            ident: syn!(syn::Ident) >>
            mut generics: syn!(syn::Generics) >>
            where_clause: option!(syn!(syn::WhereClause)) >>
            brace_inner_types: braces!(many0!(syn!(WrapperInner))) >>
            (
                WrapperEnum {
                    attrs,
                    vis,
                    enum_token,
                    ident,
                    brace_token: brace_inner_types.0,
                    inner_types: brace_inner_types.1,
                    generics: {
                        generics.where_clause = where_clause;
                        generics
                    }
                }
            )
        )
    }
}

#[derive(Clone)]
enum WrapperInner {
    Enum(WrapperInnerEnum),
    Struct(WrapperInnerStruct),
    Implicit(WrapperInnerImplicit),
}

impl From<WrapperInner> for syn::Item {
    fn from(other: WrapperInner) -> Self {
        use self::WrapperInner::*;
        match other {
            Enum(e) => e.into(),
            Struct(s) => s.into(),
            Implicit(i) => i.into(),
        }
    }
}

impl Synom for WrapperInner {
    named!{
        parse -> Self,
        alt!(
            map!(syn!(WrapperInnerEnum), WrapperInner::Enum)
            |
            map!(syn!(WrapperInnerStruct), WrapperInner::Struct)
            |
            map!(syn!(WrapperInnerImplicit), WrapperInner::Implicit)
        )
    }
}

#[derive(Clone)]
struct WrapperInnerEnum {
    item_enum:   syn::ItemEnum,
    comma_token: Option<Token![,]>,
}

impl From<WrapperInnerEnum> for syn::ItemEnum {
    fn from(other: WrapperInnerEnum) -> Self {
        other.item_enum
    }
}

impl From<WrapperInnerEnum> for syn::Item {
    fn from(other: WrapperInnerEnum) -> Self {
        other.item_enum.into()
    }
}

impl Synom for WrapperInnerEnum {
    named!{
        parse -> Self,
        do_parse!(
            item_enum: syn!(syn::ItemEnum) >>
            comma_token: option!(punct!(,)) >>
            (WrapperInnerEnum{item_enum, comma_token})
        )
    }
}

#[derive(Clone)]
struct WrapperInnerStruct {
    attrs:        Vec<syn::Attribute>,
    vis:          syn::Visibility,
    struct_token: Token![struct],
    ident:        syn::Ident,
    generics:     syn::Generics,
    fields:       syn::Fields,
    comma_token:  Option<Token![,]>,
}

impl From<WrapperInnerStruct> for syn::ItemStruct {
    fn from(other: WrapperInnerStruct) -> Self {
        let span = other.ident.span();
        let semi_token = if let syn::Fields::Named(_) = other.fields {
            None
        } else {
            Some(span.into())
        };
        syn::ItemStruct {
            attrs:        other.attrs,
            vis:          other.vis,
            struct_token: other.struct_token,
            ident:        other.ident,
            generics:     other.generics,
            fields:       other.fields,
            semi_token:   semi_token,
        }
    }
}

impl From<WrapperInnerStruct> for syn::Item {
    fn from(other: WrapperInnerStruct) -> Self {
        syn::ItemStruct::from(other).into()
    }
}

impl Synom for WrapperInnerStruct {
    named!{
        parse -> Self,
        do_parse!(
            attrs: many0!(syn::Attribute::parse_outer) >>
            vis: syn!(syn::Visibility) >>
            struct_token: option!(keyword!(struct)) >>
            ident: syn!(syn::Ident) >>
            mut generics: syn!(syn::Generics) >>
            data_struct: data_struct >>
            // _semi_token: option!(punct!(;)) >>
            comma_token: option!(punct!(,)) >>
            (
                {
                    generics.where_clause = data_struct.0;
                    let fields = data_struct.1;
                    let struct_token = struct_token.unwrap_or(ident.span().into());
                    WrapperInnerStruct {
                        attrs, vis, struct_token, ident, generics, fields, comma_token
                    }
                }
            )
        )
    }
}

#[derive(Clone)]
struct WrapperInnerImplicit {
    vis:         syn::Visibility,
    variant:     syn::Variant,
    comma_token: Option<Token![,]>,
}

impl From<WrapperInnerImplicit> for syn::ItemStruct {
    fn from(other: WrapperInnerImplicit) -> Self {
        let span = other.variant.ident.span();
        let semi_token = if let syn::Fields::Named(_) = other.variant.fields {
            None
        } else {
            Some(span.into())
        };
        syn::ItemStruct {
            attrs:        Default::default(),
            vis:          other.vis,
            struct_token: span.into(),
            ident:        other.variant.ident,
            generics:     Default::default(),
            fields:       other.variant.fields,
            semi_token:   semi_token,
        }
    }
}

impl From<WrapperInnerImplicit> for syn::Item {
    fn from(other: WrapperInnerImplicit) -> Self {
        syn::ItemStruct::from(other).into()
    }
}

impl Synom for WrapperInnerImplicit {
    named!{
        parse -> Self,
        do_parse!(
            vis: syn!(syn::Visibility) >>
            variant: syn!(syn::Variant) >>
            comma_token: option!(punct!(,)) >>
            (WrapperInnerImplicit{vis, variant, comma_token})
        )
    }
}

named!(data_struct -> (Option<syn::WhereClause>, syn::Fields), alt!(
    do_parse!(
        wh: option!(syn!(syn::WhereClause)) >>
        fields: syn!(syn::FieldsNamed) >>
        (wh, syn::Fields::Named(fields))
    )
    |
    do_parse!(
        fields: syn!(syn::FieldsUnnamed) >>
        wh: option!(syn!(syn::WhereClause)) >>
        _semi_token: option!(punct!(;)) >>
        (wh, syn::Fields::Unnamed(fields))
    )
    |
    do_parse!(
        wh: option!(syn!(syn::WhereClause)) >>
        _semi_token: option!(punct!(;)) >>
        (wh, syn::Fields::Unit)
    )
));
#[derive(Debug)]
struct DeriveTrait {
    generics: syn::Generics,
    self_ty:  syn::Type,
    blocks:   Vec<syn::Block>,
}

impl DeriveTrait {
    fn build(
        self,
        methods: Vec<syn::MethodSig>,
        trait_: syn::Path,
    ) -> syn::ItemImpl {
        assert!(methods.len() >= self.blocks.len());
        let impl_items = self
            .blocks
            .into_iter()
            .zip(methods)
            .map(|(block, sig)| {
                syn::ImplItemMethod {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    defaultness: None,
                    sig,
                    block,
                }.into()
            })
            .collect();
        syn::ItemImpl {
            attrs:       vec![],
            defaultness: None,
            unsafety:    None,
            impl_token:  Span::call_site().into(),
            generics:    self.generics,
            trait_:      Some((None, trait_, Span::call_site().into())),
            self_ty:     Box::new(self.self_ty),
            brace_token: Span::call_site().into(),
            items:       impl_items,
        }
    }
}

impl Synom for DeriveTrait {
    named!{
        parse -> Self,
        do_parse!(
            mut generics: syn!(syn::Generics) >>
            self_ty: syn!(syn::Type) >>
            where_clause: option!(syn!(syn::WhereClause)) >>
            blocks: many0!(syn!(syn::Block)) >>
            ({
                generics.where_clause = where_clause;
                DeriveTrait {
                    generics,
                    self_ty,
                    blocks,
                }
            })
        )
    }
}

struct FnArgs(syn::token::Paren, Punctuated<syn::FnArg, Token![,]>);
impl Synom for FnArgs {
    named!{
        parse -> Self,
        map!(
            parens!(Punctuated::parse_terminated),
            |(p,a)| FnArgs(p,a)
        )
    }
}

struct FnDecl(syn::Ident, syn::FnDecl);
impl From<FnDecl> for syn::MethodSig {
    fn from(other: FnDecl) -> Self {
        syn::MethodSig {
            constness: None,
            unsafety:  None,
            abi:       None,
            ident:     other.0,
            decl:      other.1,
        }
    }
}

impl Synom for FnDecl {
    named!{
        parse -> Self,
        do_parse! (
            fn_token: keyword!(fn) >>
            ident: syn!(syn::Ident) >>
            mut generics: syn!(syn::Generics) >>
            inputs: parens!(Punctuated::parse_terminated) >>
            output: syn!(syn::ReturnType) >>
            // arrow: syn!(Token![->]) >>
            // return_ty: syn!(syn::Type) >>
            where_clause: option!(syn!(syn::WhereClause)) >>
            (
                {
                    generics.where_clause = where_clause;
                    let paren_token = inputs.0;
                    let inputs = inputs.1;
                    // let output = syn::ReturnType::Type(arrow,Box::new(return_ty));
                    FnDecl(ident, syn::FnDecl{
                        fn_token,
                        generics,
                        paren_token,
                        inputs,
                        output,
                        variadic: None
                    })
                }
            )
        )
    }
}

named!{
    method_sig -> syn::MethodSig,
    map!(
        syn!(FnDecl),
        Into::into
    )
}

fn parse_to_trait_impl<T: Into<syn::MethodSig>>(
    trait_: syn::Path,
    methods: impl IntoIterator<Item = T>,
    cursor: syn::buffer::Cursor,
) -> syn::ItemImpl {
    let span = Span::call_site();
    // let buffer = syn::buffer::TokenBuffer::new2(input);
    // let cursor = buffer.begin();
    let methods = methods.into_iter().map(|ms| {
        syn::ImplItemMethod {
            attrs:       Default::default(),
            vis:         syn::Visibility::Inherited,
            defaultness: None,
            sig:         ms.into(),
            block:       syn::Block {
                brace_token: span.into(),
                stmts:       Default::default(),
            },
        }
    });
    let (mut generics, cursor) = parse_req::<syn::Generics>(cursor);
    let (ty, cursor) = parse_req::<syn::Type>(cursor);
    let (where_clause, cursor) = parse_opt(cursor);
    let (blocks, _) = parse_many::<syn::Block>(cursor);
    let methods = methods
        .zip(blocks)
        .map(|(mut m, b)| {
            m.block = b;
            m.into()
        })
        .collect();
    generics.where_clause = where_clause;
    let trait_ = Some((None, trait_, span.into()));
    syn::ItemImpl {
        attrs:       Default::default(),
        defaultness: Default::default(),
        unsafety:    Default::default(),
        impl_token:  span.into(),
        generics:    generics,
        trait_:      trait_,
        self_ty:     Box::new(ty),
        brace_token: span.into(),
        items:       methods,
    }
}

fn parse_many<T: Synom>(
    cursor: syn::buffer::Cursor,
) -> (Vec<T>, syn::buffer::Cursor) {
    let mut out = vec![];
    let mut cursor = cursor;
    while let Ok((t, cur)) = T::parse(cursor) {
        out.push(t);
        cursor = cur;
    }
    (out, cursor)
}

fn unzip_option<T, U>(opt: Option<(T, U)>) -> (Option<T>, Option<U>) {
    match opt {
        Some((t, u)) => (Some(t), Some(u)),
        None => (None, None),
    }
}

fn parse_opt<T: syn::synom::Synom>(
    cursor: syn::buffer::Cursor,
) -> (Option<T>, syn::buffer::Cursor) {
    let (result_opt, cursor_opt) = unzip_option(T::parse(cursor).ok());
    (result_opt, cursor_opt.unwrap_or(cursor))
}

fn parse_req<T: syn::synom::Synom>(
    cursor: syn::buffer::Cursor,
) -> (T, syn::buffer::Cursor) {
    T::parse(cursor).unwrap()
}

fn inner_type_enum(mut enum_: syn::ItemEnum) -> syn::ItemEnum {
    let meta = build_derive_with_span_of(
        "Eq, PartialEq, Hash, Copy, Clone, Debug, DisplayEnum",
        proc_macro2::Span::call_site(),
    );
    enum_.ident = format_ident!("{}Type", &enum_.ident);
    enum_.generics = Default::default();
    enum_.variants.iter_mut().for_each(|v| {
        v.attrs = vec![];
        v.fields = syn::Fields::Unit;
    });
    enum_.attrs.push(meta);
    enum_
}

fn push_tokens_iter<I: IntoIterator<Item = T>, T: ToTokens>(
    stream: &mut TokenStream2,
    toks: I,
) {
    toks.into_iter().for_each(|t| t.to_tokens(stream));
}

#[allow(dead_code)]
fn derive_input_as_item_enum(input: syn::DeriveInput) -> Option<syn::ItemEnum> {
    match input.data {
        syn::Data::Enum(de) => {
            Some(syn::ItemEnum {
                attrs:       input.attrs,
                vis:         input.vis,
                enum_token:  de.enum_token,
                ident:       input.ident,
                generics:    input.generics,
                brace_token: de.brace_token,
                variants:    de.variants,
            })
        },
        _ => None,
    }
}

#[allow(dead_code)]
fn item_enum_as_derive_input(item: syn::ItemEnum) -> DeriveInput {
    DeriveInput {
        attrs:    item.attrs,
        vis:      item.vis,
        ident:    item.ident,
        generics: item.generics,
        data:     syn::Data::Enum(syn::DataEnum {
            enum_token:  item.enum_token,
            brace_token: item.brace_token,
            variants:    item.variants,
        }),
    }
}

fn new_trait<M: IntoIterator<Item = T>, T: Into<syn::TraitItem>>(
    vis: &syn::Visibility,
    generics: &syn::Generics,
    trait_name: &syn::Ident,
    methods: M,
    span: Span,
) -> syn::ItemTrait {
    syn::ItemTrait {
        vis:         vis.clone(),
        ident:       trait_name.clone(),
        generics:    generics.clone(),
        items:       methods.into_iter().map(Into::into).collect(),
        trait_token: span.into(),
        brace_token: span.into(),
        unsafety:    Default::default(),
        auto_token:  Default::default(),
        colon_token: Default::default(),
        supertraits: Default::default(),
        attrs:       Default::default(),
    }
}

fn as_type_trait(vis: &syn::Visibility, ident: &syn::Ident) -> syn::ItemTrait {
    let span = ident.span();
    let sig = as_type_method_sig(ident);
    let method = trait_method_no_default(sig, span);
    let trait_name = format_ident!("As{}Type", ident);
    new_trait(vis, &Default::default(), &trait_name, Some(method), span)
}

fn trait_method_no_default(sig: syn::MethodSig, span: Span) -> syn::TraitItem {
    syn::TraitItemMethod {
        sig:        sig,
        semi_token: Some(span.into()),
        default:    Default::default(),
        attrs:      Default::default(),
    }.into()
}

fn as_type_method_sig(ident: &syn::Ident) -> syn::MethodSig {
    let ident = ident.clone();
    let span = ident.span();
    let arg = new_and_self(span).into();
    let id = format_ident!("{}Type", ident);
    let id_type = type_from_path(id.clone());
    let method_ident = format_ident_snake!("As{}Type", ident);
    let decl = new_fn_decl(Some(arg), Some(id_type), span);
    new_method_signature(method_ident, decl)
}

fn as_type_impl(
    // enum_ast: &DeriveInput,
    enum_ident: &syn::Ident,
    enum_generics: &syn::Generics,
    variant: &syn::Variant,
) -> syn::ItemImpl {
    // let enum_ident = &enum_ast.ident;
    // let enum_generics = &enum_ast.generics;
    let variant_name = &variant.ident;
    let span = variant_name.span();
    // let method_ident = format_ident_snake!("As{}Type", enum_ast.ident);
    let id = format_ident!("{}Type", enum_ident);
    // let id_type = type_from_path(id.clone());
    let trait_path = format_ident!("As{}Type", enum_ident).into();
    let generics = new_reduced_generics(enum_generics, variant);
    // let generics = reduce_bounds(&enum_generics, variant);
    let self_ty = type_from_path_and_generics(variant_name, &generics);
    // let arg = new_and_self(span).into();
    let stmt = statement_from_idents(vec![&id, &variant_name]);
    let block = block_from_statements(Some(stmt), span);
    // let decl = new_fn_decl(Some(arg), Some(id_type), span);
    // let sig = new_method_signature(method_ident, decl);
    let sig = as_type_method_sig(enum_ident);
    let method = new_impl_method(sig, block);
    new_impl_trait(generics, trait_path, self_ty, Some(method), span)
}

fn block_from_statements<S: IntoIterator<Item = syn::Stmt>>(
    stmts: S,
    span: Span,
) -> syn::Block {
    syn::Block {
        brace_token: span.into(),
        stmts:       stmts.into_iter().collect(),
    }
}

fn statement_from_idents<'a, T: IntoIterator<Item = &'a syn::Ident>>(
    ids: T,
) -> syn::Stmt {
    syn::Stmt::Expr(syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path:  syn::Path {
            leading_colon: None,
            segments:      ids
                .into_iter()
                .cloned()
                .map(syn::PathSegment::from)
                .collect(),
        },
    }))
}

fn generic_arg_from_generic_param(
    param: syn::GenericParam,
) -> syn::GenericArgument {
    use syn::GenericParam::*;
    match param {
        Type(syn::TypeParam {
            ident, ..
        }) => syn::GenericArgument::Type(type_from_path(ident)),
        Lifetime(syn::LifetimeDef {
            lifetime, ..
        }) => syn::GenericArgument::Lifetime(lifetime),
        Const(_) => panic!("Must be generic type or lifetime"),
    }
}

fn type_from_path_and_generics(
    ident: &syn::Ident,
    generics: &syn::Generics,
) -> syn::Type {
    let span = ident.span();
    let args = generics
        .clone()
        .params
        .into_iter()
        .map(generic_arg_from_generic_param);
    syn::TypePath {
        qself: None,
        path:  syn::Path::from(syn::PathSegment {
            ident:     ident.clone(),
            arguments: path_arg_from_params(args, span),
        }),
    }.into()
}

fn path_arg_from_params<P: IntoIterator<Item = syn::GenericArgument>>(
    args: P,
    span: Span,
) -> syn::PathArguments {
    let args = args.into_iter().collect::<Vec<_>>();
    if args.len() > 0 {
        syn::PathArguments::AngleBracketed(
            syn::AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token:     span.into(),
                args:         args.into_iter().collect(),
                gt_token:     span.into(),
            },
        )
    } else {
        syn::PathArguments::None
    }
}

fn type_from_path<P: Into<syn::Path>>(path: P) -> syn::Type {
    syn::TypePath {
        path:  path.into(),
        qself: Default::default(),
    }.into()
}

fn new_and_self(span: Span) -> syn::ArgSelfRef {
    syn::ArgSelfRef {
        and_token:  span.into(),
        self_token: span.into(),
        lifetime:   Default::default(),
        mutability: Default::default(),
    }
}

#[allow(dead_code)]
fn new_and_mut_self(span: Span) -> syn::ArgSelfRef {
    syn::ArgSelfRef {
        mutability: Some(span.into()),
        ..new_and_self(span)
    }
}

fn new_fn_decl<T: IntoIterator<Item = syn::FnArg>>(
    args: T,
    maybe_output_type: Option<syn::Type>,
    span: Span,
) -> syn::FnDecl {
    let output = match maybe_output_type {
        None => syn::ReturnType::Default,
        Some(ty) => syn::ReturnType::Type(span.into(), ty.into()),
    };
    syn::FnDecl {
        fn_token:    span.into(),
        paren_token: span.into(),
        inputs:      args.into_iter().collect(),
        output:      output,
        generics:    Default::default(),
        variadic:    Default::default(),
    }
}

fn new_impl_method(
    sig: syn::MethodSig,
    block: syn::Block,
) -> syn::ImplItemMethod {
    syn::ImplItemMethod {
        attrs:       vec![],
        vis:         syn::Visibility::Inherited,
        defaultness: None,
        sig:         sig,
        block:       block,
    }
}

fn new_method_signature(
    ident: syn::Ident,
    decl: syn::FnDecl,
) -> syn::MethodSig {
    syn::MethodSig {
        ident:     ident,
        decl:      decl,
        constness: Default::default(),
        unsafety:  Default::default(),
        abi:       Default::default(),
    }
}

fn new_impl_trait<T: IntoIterator<Item = I>, I: Into<syn::ImplItem>>(
    generics: syn::Generics,
    trait_path: syn::Path,
    self_ty: syn::Type,
    items: T,
    span: Span,
) -> syn::ItemImpl {
    syn::ItemImpl {
        impl_token:  span.into(),
        generics:    generics,
        trait_:      Some((None, trait_path, span.into())),
        self_ty:     self_ty.into(),
        brace_token: span.into(),
        items:       items.into_iter().map(Into::into).collect(),
        attrs:       Default::default(),
        defaultness: Default::default(),
        unsafety:    Default::default(),
    }
}

#[allow(dead_code)]
fn vis_at_span<T: Spanned>(vis: &syn::Visibility, s: &T) -> syn::Visibility {
    let span = s.span();
    use syn::Visibility::*;
    match vis {
        Public(_) => {
            Public(syn::VisPublic {
                pub_token: span.into(),
            })
        },
        Crate(_) => {
            Crate(syn::VisCrate {
                crate_token: span.into(),
            })
        },
        Restricted(syn::VisRestricted {
            in_token,
            path,
            ..
        }) => {
            let in_token = in_token.map(|_| span.into());
            Restricted(syn::VisRestricted {
                pub_token: span.into(),
                paren_token: span.into(),
                in_token,
                path: path.clone(),
            })
        },
        Inherited => Inherited,
    }
}

fn build_derive_with_span_of(derives: &str, span: Span) -> syn::Attribute {
    let path: syn::Path = syn::Ident::new("derive", span).into();
    let nested: Punctuated<_, Token![,]> = derives
        .split(',')
        .map(|id| {
            syn::NestedMeta::Meta(syn::Meta::Word(syn::Ident::new(
                id.trim(),
                span,
            )))
        })
        .collect();
    let mut tts = TokenStream2::new();
    syn::token::Paren::from(span)
        .surround(&mut tts, |toks| nested.to_tokens(toks));
    // syn::MetaList {
    //     ident: syn::Ident::new("derive", span),
    //     paren_token: span.into(),
    //     nested
    // }.into()
    syn::Attribute {
        pound_token: span.into(),
        style: syn::AttrStyle::Outer,
        bracket_token: span.into(),
        path,
        tts,
        is_sugared_doc: false,
    }
}

fn ensure_enum(ast: &DeriveInput) -> &syn::DataEnum {
    match ast.data {
        Data::Enum(ref de) => de,
        _ => panic!("Entity must be an enum!"),
    }
}

fn _unreference(mut ty: &syn::Type) -> &syn::Type {
    while let syn::Type::Reference(r) = ty {
        ty = &*r.elem;
    }
    ty
}

fn build_struct_item_from_variant(
    vis: &syn::Visibility,
    generics: &syn::Generics,
    variant: &syn::Variant,
) -> syn::ItemStruct {
    let generics = new_reduced_generics(generics, variant);
    let vis = vis.clone();
    let syn::Variant {
        attrs,
        ident,
        fields,
        ..
    } = variant.clone();
    let struct_token = ident.span().into();
    let mut semi_token = None;
    if let syn::Fields::Unnamed(_) = fields {
        semi_token = Some(ident.span().into())
    }
    syn::ItemStruct {
        attrs,
        ident,
        fields,
        generics,
        vis,
        semi_token,
        struct_token,
    }
}

trait Strip<'a, T: 'a, P: Default>: IntoIterator<Item = &'a T> {
    fn strip(
        self,
        keep: &Vec<GenericType>,
    ) -> syn::punctuated::Punctuated<&'a T, P>;
}

impl<'a, T: 'a, P: Default> Strip<'a, T, P> for &'a Punctuated<T, P>
where
    &'a T: Into<GenericType<'a>>,
{
    fn strip(self, keep: &Vec<GenericType>) -> Punctuated<&'a T, P> {
        self.into_iter()
            .filter(|&t| keep.contains(&t.into()))
            .collect()
    }
}

fn mutate_variant_to_container(
    generics: &syn::Generics,
    variant: &mut syn::Variant,
) {
    // let gens = reduce_bounds(generics, variant);
    let mut gens = generics.clone();
    reduce_generics(&mut gens, variant);
    // let (_, ty_generics, _) = gens.split_for_impl();
    let ty = type_from_path_and_generics(&variant.ident, &gens);
    let span = variant.ident.span();
    variant.attrs = Default::default();
    variant.fields = syn::Fields::Unnamed(syn::FieldsUnnamed {
        paren_token: span.into(),
        unnamed:     Some(syn::Field {
            vis:         syn::Visibility::Inherited,
            ty:          ty,
            attrs:       Default::default(),
            colon_token: Default::default(),
            ident:       Default::default(),
        }).into_iter()
            .collect(),
    });
}

fn make_container_item(mut enum_: syn::ItemEnum) -> syn::ItemEnum {
    let generics = enum_.generics.clone();
    enum_
        .variants
        .iter_mut()
        .for_each(|v| mutate_variant_to_container(&generics, v));
    enum_
}

fn generic_types_and_lifetimes<'a>(
    generics: &'a syn::Generics,
) -> Vec<GenericType<'a>> {
    let mut out = Vec::new();
    generics.params.iter().for_each(|gp| {
        use syn::GenericParam::*;
        match gp {
            Type(tp) => out.push(GenericType::Type(&tp.ident)),
            Lifetime(l) => out.push(GenericType::Lifetime(&l.lifetime.ident)),
            _ => (),
        }
    });
    out
}

fn types_to_match_against<'a, P>(
    params: &'a Punctuated<syn::GenericParam, P>,
    bound: &'a syn::TraitBound,
) -> impl Iterator<Item = GenericType<'a>> {
    let generic_types =
        params.iter().filter_map(param_type).collect::<Vec<_>>();
    bound
        .path
        .as_generic()
        .into_iter()
        .filter(move |gt| generic_types.contains(gt))
}

fn types_to_match_against_generic<'a, P>(
    params: &'a Punctuated<syn::GenericParam, P>,
    ty: &'a impl AsGenerics<'a>,
) -> impl Iterator<Item = GenericType<'a>> {
    let generic_types =
        params.iter().filter_map(param_type).collect::<Vec<_>>();
    ty.as_generic()
        .into_iter()
        .filter(move |gt| generic_types.contains(gt))
}

fn trait_bound_relevance<'a, P>(
    params: &'a Punctuated<syn::GenericParam, P>,
    bound: &'a syn::TraitBound,
    variant: &syn::Variant,
) -> bool {
    types_to_match_against(params, bound)
        .all(|ref t| variant.as_generic().contains(t))
}

fn param_type<'a>(
    generic_param: &'a syn::GenericParam,
) -> Option<GenericType<'a>> {
    use syn::GenericParam::*;
    match generic_param {
        Type(tp) => Some(GenericType::Type(&tp.ident)),
        Lifetime(l) => Some(GenericType::Lifetime(&l.lifetime.ident)),
        _ => None,
    }
}

fn filter_mut<T>(
    params: &mut Punctuated<T, impl Default>,
    f: impl Fn(&T) -> bool,
) {
    let orig = std::mem::replace(params, Default::default());
    *params = orig.into_iter().filter(f).collect();
}

fn param_bound_relevance<'a, P>(
    bound: &syn::TypeParamBound,
    params: &'a Punctuated<syn::GenericParam, P>,
    variant: &syn::Variant,
) -> bool {
    use syn::TypeParamBound::*;
    match bound {
        Trait(tb) => trait_bound_relevance(&params, tb, variant),
        Lifetime(l) => variant.as_generic().contains(&l.into()),
    }
}

fn new_reduced_generics(
    generics: &syn::Generics,
    variant: &syn::Variant,
) -> syn::Generics {
    let mut generics = generics.clone();
    reduce_generics(&mut generics, variant);
    generics
}

fn reduce_generics(generics: &mut syn::Generics, variant: &syn::Variant) {
    let variant_generics = variant.as_generic();
    let generic_params = generics.params.clone();
    generics.params.iter_mut().for_each(|p| {
        match p {
            syn::GenericParam::Type(type_param) => {
                filter_mut(&mut type_param.bounds, |_b| {
                    param_bound_relevance(_b, &generic_params, variant)
                    // use syn::TypeParamBound::*;
                    // // _b.as_generic()
                    // //     .iter()
                    // //     .all(|i| variant_generics.contains(i));
                    // match _b {
                    //     Trait(tb) => {
                    //         trait_bound_relevance(&generic_params, tb, variant)
                    //     },
                    //     Lifetime(l) => variant_generics.contains(&l.into()),
                    // }
                });
                if type_param.bounds.iter().count() == 0 {
                    type_param.colon_token = None;
                }
            },
            syn::GenericParam::Lifetime(l) => {
                filter_mut(&mut l.bounds, |_b| {
                    _b.as_generic().iter().all(|i| variant_generics.contains(i))
                });
                if l.bounds.iter().count() == 0 {
                    l.colon_token = None;
                }
            },
            _ => (),
        }
    });
    if let Some(ref mut wc) = generics.where_clause {
        wc.predicates.iter_mut().for_each(|wp| {
            match wp {
                syn::WherePredicate::Type(pt) => {
                    filter_mut(&mut pt.bounds, |pb| {
                        param_bound_relevance(pb, &generic_params, variant)
                    });
                },
                syn::WherePredicate::Lifetime(pl) => {
                    filter_mut(&mut pl.bounds, |l| {
                        variant.as_generic().contains(&l.into())
                    });
                    if pl.bounds.iter().count() == 0 {
                        pl.colon_token = None;
                    }
                },
                _ => {},
            }
        });
        filter_mut(&mut wc.predicates, |wp| {
            use syn::WherePredicate::*;
            match wp {
                Type(pt) => {
                    pt.bounds.iter().count() > 0
                        && types_to_match_against_generic(
                            &generic_params,
                            &pt.bounded_ty,
                        ).all(|ref t| variant_generics.contains(t))
                },
                Lifetime(pl) => {
                    pl.bounds.iter().count() > 0
                        && variant_generics.contains(&(&pl.lifetime).into())
                },
                _ => false,
            }
        })
    }
    filter_mut(&mut generics.params, |p| {
        if let Some(ref ty) = param_type(p) {
            variant_generics.contains(ty)
        } else {
            true
        }
    });
    // generics.where_clause.iter_mut();
}

impl<'a> AsGenerics<'a> for syn::TypeParamBound {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::TypeParamBound::Trait(tb) => tb.path.as_generic(),
            syn::TypeParamBound::Lifetime(l) => l.as_generic(),
        }
    }
}

impl<'a> AsGenerics<'a> for syn::Variant {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        self.fields.as_generic()
    }
}

impl<'a> AsGenerics<'a> for syn::Fields {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::Fields::Named(syn::FieldsNamed {
                named: fields, ..
            })
            | syn::Fields::Unnamed(syn::FieldsUnnamed {
                unnamed: fields,
                ..
            }) => fields.iter().flat_map(|f| f.as_generic()).collect(),
            syn::Fields::Unit => vec![],
        }
    }
}

impl<'a> AsGenerics<'a> for syn::Field {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        self.ty.as_generic()
    }
}

impl<'a> AsGenerics<'a> for syn::Generics {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        self.params.iter().flat_map(|p| p.as_generic()).collect()
    }
}

impl<'a> AsGenerics<'a> for syn::GenericParam {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        use syn::GenericParam::*;
        match self {
            Type(tp) => tp.as_generic(),
            Lifetime(ld) => ld.as_generic(),
            Const(_) => vec![],
        }
    }
}

impl<'a> AsGenerics<'a> for syn::TypeParam {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        vec![self::GenericType::new_from_type_ident(&self.ident)]
    }
}

impl<'a> AsGenerics<'a> for syn::LifetimeDef {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        vec![self::GenericType::new_from_lifetime_ident(
            &self.lifetime.ident,
        )]
    }
}

impl<'a> AsGenerics<'a> for syn::Type {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        use syn::Type::*;
        match self {
            Slice(syn::TypeSlice {
                elem: t, ..
            })
            | Array(syn::TypeArray {
                elem: t, ..
            })
            | Ptr(syn::TypePtr {
                elem: t, ..
            })
            | Paren(syn::TypeParen {
                elem: t, ..
            })
            | Group(syn::TypeGroup {
                elem: t, ..
            }) => t.as_generic(),
            Tuple(syn::TypeTuple {
                elems: ts, ..
            }) => ts.iter().flat_map(|t| t.as_generic()).collect(),
            Reference(syn::TypeReference {
                lifetime,
                elem: t,
                ..
            }) => {
                let mut out = lifetime.as_generic();
                out.append(&mut t.as_generic());
                out
            },
            Path(syn::TypePath {
                path: p, ..
            }) => p.as_generic(),
            Never(_) | Macro(_) | Verbatim(_) | Infer(_) => vec![],
            BareFn(_) | TraitObject(_) | ImplTrait(_) => vec![],
        }
    }
}

impl<'a> AsGenerics<'a> for syn::Lifetime {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        vec![GenericType::new_from_lifetime_ident(&self.ident)]
    }
}

impl<'a> AsGenerics<'a> for syn::Path {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        let mut segs = self.segments.iter().collect::<Vec<_>>();
        match segs.pop() {
            Some(seg) => {
                if seg.arguments.is_empty() {
                    if segs.len() > 0 {
                        return vec![];
                    }
                    vec![GenericType::new_from_type_ident(&seg.ident)]
                } else {
                    seg.arguments.as_generic()
                }
            },
            None => vec![],
        }
    }
}

impl<'a> AsGenerics<'a> for syn::PathArguments {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        use syn::PathArguments::*;
        match self {
            None => vec![],
            AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) => args.iter().flat_map(|a| a.as_generic()).collect(),
            Parenthesized(syn::ParenthesizedGenericArguments {
                inputs,
                output,
                ..
            }) => {
                inputs
                    .iter()
                    .flat_map(|t| t.as_generic())
                    .chain(match output {
                        syn::ReturnType::Default => vec![],
                        syn::ReturnType::Type(_, ty) => ty.as_generic(),
                    })
                    .collect()
            },
        }
    }
}

impl<'a> AsGenerics<'a> for syn::GenericArgument {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::GenericArgument::Lifetime(l) => {
                vec![GenericType::Lifetime(&l.ident)]
            },
            syn::GenericArgument::Type(syn::Type::Path(tp)) => {
                tp.path
                    .segments
                    .iter()
                    .map(|s| GenericType::Type(&s.ident))
                    .collect()
            },
            syn::GenericArgument::Binding(_) => unimplemented!(),
            _ => panic!("No idea what these are"),
        }
    }
}

impl<'a, T: AsGenerics<'a>> AsGenerics<'a> for Option<T> {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            Some(t) => t.as_generic(),
            None => vec![],
        }
    }
}

#[derive(Copy, Clone)]
crate enum GenericType<'a> {
    Type(&'a syn::Ident),
    Lifetime(&'a syn::Ident),
}

impl<'a> std::fmt::Debug for GenericType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GenericType::Lifetime(id) => write!(f, "'{}", id),
            GenericType::Type(id) => write!(f, "{}", id),
        }
    }
}

impl<'a> PartialEq for GenericType<'a> {
    fn eq(&self, other: &Self) -> bool {
        use self::GenericType::*;
        match (self, other) {
            (Type(l), Type(r)) => r.to_string() == l.to_string(),
            (Lifetime(l), Lifetime(r)) => r.to_string() == l.to_string(),
            _ => false,
        }
    }
}

impl<'a> PartialEq<syn::GenericArgument> for GenericType<'a> {
    fn eq(&self, other: &syn::GenericArgument) -> bool {
        self.as_generic() == other.as_generic()
    }
}

#[allow(dead_code)]
impl<'a> GenericType<'a> {
    fn as_ident(&self) -> &syn::Ident {
        use self::GenericType::*;
        match self {
            Type(id) | Lifetime(id) => id,
        }
    }
    fn as_tokens(&self) -> proc_macro2::TokenStream {
        match self {
            GenericType::Type(id) => quote_spanned!(id.span()=> #id),
            GenericType::Lifetime(id) => {
                let life = syn::Lifetime::new(&format!("'{}", id), id.span());
                quote_spanned!(life.span()=> #life)
            },
        }
    }
    fn new_from_type_ident(id: &'a syn::Ident) -> Self {
        GenericType::Type(id)
    }
    fn new_from_lifetime_ident(id: &'a syn::Ident) -> Self {
        GenericType::Lifetime(id)
    }
    fn is_lifetime(&self) -> bool {
        if let GenericType::Lifetime(_) = self {
            true
        } else {
            false
        }
    }
    fn lifetimes_and_types(vec: Vec<Self>) -> (Vec<Self>, Vec<Self>) {
        vec.into_iter().partition(|e| e.is_lifetime())
    }
}

impl<'a> From<&'a syn::Lifetime> for GenericType<'a> {
    fn from(other: &'a syn::Lifetime) -> Self {
        GenericType::Lifetime(&other.ident)
    }
}

impl<'a> AsGenerics<'a> for GenericType<'a> {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        vec![*self]
    }
}

trait AsGenerics<'a> {
    fn as_generic(&'a self) -> Vec<GenericType<'a>>;
}

#[allow(dead_code)]
crate fn intersection<'a, T: 'a + PartialEq>(
    a: impl IntoIterator<Item = &'a T>,
    b: impl IntoIterator<Item = &'a T>,
) -> impl IntoIterator<Item = &'a T> {
    let bs: Vec<_> = b.into_iter().collect();
    a.into_iter().filter(move |e| bs.contains(e))
}

named!{
    inner_items -> Vec<WrapperInner>,
    many0!(syn!(WrapperInner))
}

#[allow(dead_code)]
#[cfg(test)]
mod tests {
    use super::{
        AsGenerics,
        GenericType,
        *,
    };
    use syn::synom::Parser;
    #[test]
    fn test_wrapper() {
        let _inner_struct: WrapperInnerStruct =
            syn::parse2(quote!{
                struct TupleLike(u8, u16),
            }).expect("WrapperInnerStruct");
        let _inner_implicit: WrapperInnerImplicit =
            syn::parse2(quote!{
                TupleLike(u8, u16),
            }).expect("WrapperInnerImplicit");
        let _inner_enum: WrapperInnerEnum = syn::parse2(quote!{
            enum EnumLike{Var1(u8), Unit},
        }).expect("WrapperInnerEnum");
        let _inner: WrapperInner = syn::parse2(quote!{
            enum EnumLike{Var1(u8), Unit},
        }).expect("WrapperInner");
        let wrapper: WrapperEnum = syn::parse2(quote!{
        // syn::parse2(quote!{
            pub enum Inner<T, U> {
            struct StructLike {
                named_field: str,
                other_field: u8
            },
            TupleLike(u16, String),
            UnitLike,
            enum InnerEnum<T, U> {
                UnitVar,
                TupleVar(u32),
                StructVar{field: u64}
            }
        }
        }).expect("WrapperEnum");
        let innards = wrapper.as_items();
        println!("\n{}\n", quote!(#(#innards)*));
    }
    #[test]
    fn test_synom() {
        let q = quote!(fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result);
        // println!("{:#?}", q);
        let fmt_sig: Result<FnDecl, _> = syn::parse2(q);
        syn::parse_str::<FnArgs>("(&self, f: &mut std::fmt::Formatter)")
            .expect("FnArgs");
        syn::parse_str::<FnDecl>(
            "fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result",
        ).expect("FnDecl");
        let m_sig = method_sig
            .parse_str(
                "fn fmt(&self, f: &mut std::fmt::Formatter) -> \
                 std::fmt::Result",
            )
            .expect("MethodSig");
        let m_sig2 = method_sig
            .parse_str(
                "fn second_fn(&self, f: &mut std::fmt::Formatter) -> \
                 std::fmt::Result",
            )
            .expect("MethodSig");
        // println!("{:#?}", m_sig);
        let derive_trait = syn::parse_str::<DeriveTrait>(
            "<T, U> TypeName<T,U> where T: Clone {panic!();} \
             {unimplemented!();}",
        ).unwrap();
        let default_path: syn::Path =
            syn::parse_str("std::fmt::Default").unwrap();
        let default_impl =
            derive_trait.build(vec![m_sig, m_sig2], default_path);
        println!("\n{}\n", quote!(#default_impl));
        // println!("{:#?}", derive_trait);
        // if let Err(ref e) = fmt_sig {
        //     println!("{:#?}", e);
        // }
        let fmt_sig = syn::MethodSig::from(fmt_sig.unwrap());
        println!("{}", quote!(#fmt_sig));
    }
    // #[test]
    fn test_reduce_generics() {
        let q = quote!{
            struct Struc<'a, 'b, T: SomeTrait<U> + 'b, U: 'a + SomeTrait<T> + SomeOtherTrait<String> + SomeThirdTrait<str, T>> (T, &'a U) where T: Clone;
        };
        let mut struct_ = syn::parse2::<syn::ItemStruct>(q).unwrap();
        let e = quote!{
            enum Enum {
                Variant(&'a U)
            }
        };
        let mut enum_ = syn::parse2::<syn::ItemEnum>(e).unwrap();
        let variant = enum_.variants.iter().next().unwrap();
        reduce_generics(&mut struct_.generics, variant);
        enum_.generics = struct_.generics.clone();
        println!("\n{}\n", quote!(#enum_));
    }
    // #[test]
    fn test_types() {
        let q = quote!{
            pub struct Ty<'a, 'b: 'a, T: 'a + 'b, U>(&'a T, std::option::Option<U>, usize, &'a &'b (&'b T, U));
        };
        let ast: DeriveInput = syn::parse2(q).unwrap();
        let data = &ast.data;
        if let syn::Data::Struct(s) = data {
            if let syn::Fields::Unnamed(u) = &s.fields {
                for ty in u.unnamed.iter().map(|f| &f.ty) {
                    use syn::Type::*;
                    println!(
                        "{}",
                        match ty {
                            Verbatim(_) => "verbatim",
                            Group(_) => "Group",
                            Paren(_) => "Paren",
                            Tuple(_) => "Paren",
                            Path(_) => "Path",
                            Reference(_) => "Reference",
                            TraitObject(_) => "TraitObject",
                            _ => "Other",
                        }
                    );
                    let a_lifetime = syn::Ident::new("a", ty.span());
                    let u_type = syn::Ident::new("U", ty.span());
                    let c_lifetime = syn::Ident::new("c", ty.span());
                    let union_test: Vec<GenericType> = vec![
                        GenericType::new_from_lifetime_ident(&a_lifetime),
                        GenericType::new_from_lifetime_ident(&c_lifetime),
                        GenericType::new_from_type_ident(&u_type),
                    ];
                    println!("generics: {:?} ++", ty.as_generic());
                    let gen_tys = ty.as_generic();
                    println!(
                        "union: {:?}",
                        super::intersection(&union_test, &gen_tys)
                            .into_iter()
                            .collect::<Vec<_>>()
                    );
                }
            }
        }
    }
    // #[test]
    fn test_split_for_impl() {
        let q = quote!{
            pub struct Split<'a, 'b, T: Clone, U>
            where U: 'b,
            'b: 'a,
            {d: T, e: &'a &'b U}
        };
        let ast: syn::DeriveInput = syn::parse2(q).unwrap();
        let (a, b, c) = ast.generics.split_for_impl();
        println!("{}", quote!(#a #b #c));
    }
    // #[test]
    fn test_display_enum() {
        let disp = quote!{
            pub enum Test {
                VariantOne,
                OtherVariant,
                Third,
            }
        };
        // let stream: ::proc_macro::TokenStream = disp.into();
        // let ts: proc_macro2::TokenStream = disp.clone().into();
        // let ts: DeriveInput = syn::parse2(ts).unwrap();
        println!("{}", disp);
        println!("{}", ::display_enum_tokenstream_2(disp));
    }
    // #[test]
    fn test_as_type() {
        let ident = syn::Ident::new("TraitName", Span::call_site());
        let trait_ = as_type_trait(&syn::Visibility::Inherited, &ident);
        println!("\n---\n{}\n---\n", quote!(#trait_));
        let parsed = syn::parse2::<syn::Item>(quote!(#trait_)).unwrap();
        println!("{:?}", parsed);
        assert_eq!(parsed, trait_.into());
    }
    // #[test]
    fn test_item() {
        let enm = quote!{
            pub enum Test {
                VariantOne,
                OtherVariant,
                Third,
            }
        };
        let try_parse: syn::Item = syn::parse2(enm).unwrap();
        println!("{:?}", try_parse);
        println!("{}", quote!(#try_parse));
    }
}
