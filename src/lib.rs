#![feature(
    rust_2018_edition, rustc_private, proc_macro, extern_prelude,
    iterator_flatten
)]
#![allow(dead_code)]

#[macro_use]
extern crate quote;

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
    let ast: DeriveInput = syn::parse2(input).unwrap();
    let name = &ast.ident;
    let vis = &ast.vis;
    let enum_data = ensure_enum(&ast);



    let var_names = variant_names(enum_data).into_iter().collect::<Vec<_>>();
    let var_names_iter = var_names.iter();
    let id_name: syn::Ident =
        syn::Ident::new(&format!("{}Type", name).to_camel_case(), name.span());
    let enum_token: Token![enum] = name.span().into();
    let meta = build_derive_with_span_of(
        "Eq, PartialEq, Hash, Copy, Clone, Debug, DisplayEnum",
        &id_name,
    );
    let vis_s = vis_at_span(&vis, &id_name);
    let id_enum = quote!{ //_spanned!{name.span()=>
        // #[derive(Eq, PartialEq, Hash, Copy, Clone, Debug, DisplayEnum)]
        #meta
        #vis_s #enum_token #id_name { #(#var_names_iter,)* }
    };
    // let trait_name_string = format!("As{}", id_name);
    // let trait_name =
    //     syn::Ident::new(&trait_name_string.to_camel_case(), name.span());
    // let fn_name =
    //     syn::Ident::new(&trait_name_string.to_snake_case(), name.span());
    // let fn_token: Token![fn] = name.span().into();
    // let trait_token: Token![trait] = name.span().into();
    // let as_trait = quote!{ //_spanned!{name.span()=>
    //     #vis #trait_token #trait_name {
    //         #fn_token #fn_name(&self) -> #id_name;
    //     }
    // };

    let as_trait = as_type_trait(vis, name);


    let inner_structs = enum_data
        .variants
        .iter()
        .map(|v| {
            let var_struct = build_struct_from_variant(&ast, &v);

            let type_impl = as_type_impl(&ast, &v);

            quote!{
                #var_struct
                #type_impl
            }
        })
        .collect::<Vec<_>>();
    let inner_struct_iter = inner_structs.iter();

    let container = make_container(&ast);

    let out = quote!{ //_spanned!{ast.span()=>
        #id_enum
        #(#inner_struct_iter)*
        #as_trait
        #container
    };

    // panic!("{}", out);
    out.into()
}

fn new_trait<M: IntoIterator<Item = T>, T: Into<syn::TraitItem>>(
    vis: &syn::Visibility,
    generics: &syn::Generics,
    trait_name: &syn::Ident,
    methods: M,
    span: Span,
) -> syn::Item {
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
    }.into()
}

fn as_type_trait(vis: &syn::Visibility, ident: &syn::Ident) -> syn::Item {
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
    enum_ast: &DeriveInput,
    variant: &syn::Variant,
) -> syn::ItemImpl {
    let enum_ident = &enum_ast.ident;
    let variant_name = &variant.ident;
    let span = variant_name.span();
    // let method_ident = format_ident_snake!("As{}Type", enum_ast.ident);
    let id = format_ident!("{}Type", enum_ast.ident);
    // let id_type = type_from_path(id.clone());
    let trait_path = format_ident!("As{}Type", enum_ast.ident).into();
    let generics = reduce_bounds(enum_ast, variant);
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
        Type(syn::TypeParam { ident, .. }) => {
            syn::GenericArgument::Type(type_from_path(ident))
        },
        Lifetime(syn::LifetimeDef { lifetime, .. }) => {
            syn::GenericArgument::Lifetime(lifetime)
        },
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

// fn empty_generics() -> syn::Generics {
//     syn::Generics {
//         params: empty_punctuted(),
//         ..Default::default()
//     }
// }

// fn empty_punctuted<T, P: Default>() -> Punctuated<T, P> {
//     let none: Option<T> = None;
//     none.into_iter().collect()
// }

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
        Restricted(syn::VisRestricted { in_token, path, .. }) => {
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

fn build_derive_with_span_of<T: Spanned>(
    derives: &str,
    span: &T,
) -> syn::Attribute {
    let span = span.span();
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


fn variant_names(e: &syn::DataEnum) -> impl IntoIterator<Item = &syn::Ident> {
    e.variants.iter().map(|v| &v.ident)
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

fn build_struct_from_variant(
    enum_ast: &DeriveInput,
    variant: &syn::Variant,
) -> syn::DeriveInput {
    let generics = reduce_bounds(enum_ast, variant);
    let ident = variant.ident.clone();
    let vis = enum_ast.vis.clone();
    let fields = variant.fields.clone();
    let attrs = variant.attrs.clone();
    let struct_token = ident.span().into();
    let mut semi_token = None;
    if let syn::Fields::Unnamed(_) = fields {
        semi_token = Some(ident.span().into())
    }
    let data_struct = syn::DataStruct {
        struct_token,
        fields,
        semi_token,
    };
    syn::DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data: syn::Data::Struct(data_struct),
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

fn container_variant(
    enum_ast: &DeriveInput,
    struct_ast: &syn::Variant,
) -> syn::Variant {
    let gens = reduce_bounds(enum_ast, struct_ast);
    let (_, ty_generics, _) = gens.split_for_impl();

    let ident = struct_ast.ident.clone();
    let paren_token = struct_ast.span().into();
    let ty: syn::Type = syn::parse2(quote!(#ident #ty_generics)).unwrap();
    // let ty: syn::Type = syn::parse2(quote_spanned!(ident.span()=> #ident #ty_generics)).unwrap();
    let unnamed = Some(syn::Field {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        ident: None,
        colon_token: None,
        ty,
    }).into_iter()
        .collect();
    syn::Variant {
        attrs: vec![],
        ident,
        discriminant: None,
        fields: syn::Fields::Unnamed(syn::FieldsUnnamed {
            paren_token,
            unnamed,
        }),
    }
}

fn make_container(enum_ast: &DeriveInput) -> DeriveInput {
    let data = ensure_enum(enum_ast);
    let vis = &enum_ast.vis;
    let variants = data.variants.iter().map(|v| container_variant(enum_ast, v));
    let ident = enum_ast.ident.clone();
    let enum_token: Token![enum] = ident.span().into();
    let (tys, _, wh) = enum_ast.generics.split_for_impl();
    syn::parse2(quote!{ //_spanned!{enum_ast.span()=>
        #vis
        #enum_token #ident #tys #wh {
            #(#variants),*
        }
    }).unwrap()
}

fn reduce_bounds(
    enum_ast: &DeriveInput,
    struct_ast: &syn::Variant,
) -> syn::Generics {
    let struct_generics = struct_ast.as_generic();
    let reduced_where_bounds = enum_ast
        .generics
        .where_clause
        .iter()
        .flat_map(|w| w.predicates.iter())
        .filter_map(|w| {
            match w.clone() {
                syn::WherePredicate::Lifetime(syn::PredicateLifetime {
                    lifetime,
                    colon_token,
                    bounds,
                }) => {
                    if struct_generics.contains(&(&lifetime).into()) {
                        let bounds: Punctuated<_, _> = bounds
                            .strip(&struct_generics)
                            .into_iter()
                            .cloned()
                            .collect();
                        if bounds.iter().count() == 0 {
                            None
                        } else {
                            Some(syn::WherePredicate::Lifetime(
                                syn::PredicateLifetime {
                                    lifetime,
                                    colon_token,
                                    bounds,
                                },
                            ))
                        }
                    } else {
                        None
                    }
                },
                syn::WherePredicate::Type(syn::PredicateType {
                    bounded_ty,
                    colon_token,
                    bounds,
                    ..
                }) => {
                    let relevant = {
                        let bounded_types = bounded_ty.as_generic();
                        bounded_types.len() > 0
                            && bounded_types
                                .iter()
                                .all(|t| struct_generics.contains(t))
                    };

                    if relevant {
                        let bounds = bounds
                            .into_iter()
                            .filter_map(|b| {
                                {
                                    if let syn::TypeParamBound::Lifetime(l) = &b
                                    {
                                        if struct_generics.contains(&l.into()) {
                                            Some(0)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }.map(move |_| b)
                            })
                            .collect::<Punctuated<_, _>>();
                        if bounds.iter().count() == 0 {
                            None
                        } else {
                            Some(syn::WherePredicate::Type(
                                syn::PredicateType {
                                    bounded_ty,
                                    colon_token: colon_token,
                                    bounds,
                                    lifetimes: None,
                                },
                            ))
                        }
                    } else {
                        None
                    }
                },
                _ => None,
            }
        });
    let where_clause = Some(syn::WhereClause {
        where_token: syn::token::Where::default(),
        predicates:  reduced_where_bounds.collect(),
    });
    let params = enum_ast
        .generics
        .params
        .iter()
        .filter_map(|p| {
            match p.clone() {
                syn::GenericParam::Lifetime(syn::LifetimeDef {
                    lifetime,
                    bounds,
                    attrs,
                    colon_token,
                }) => {
                    if struct_generics.contains(&(&lifetime).into()) {
                        let bounds = bounds
                            .strip(&struct_generics)
                            .into_iter()
                            .cloned()
                            .collect();
                        Some(syn::GenericParam::Lifetime(syn::LifetimeDef {
                            lifetime,
                            attrs,
                            colon_token,
                            bounds,
                        }))
                    } else {
                        None
                    }
                },
                syn::GenericParam::Type(syn::TypeParam {
                    ident,
                    colon_token,
                    bounds,
                    ..
                }) => {
                    if struct_generics
                        .contains(&GenericType::new_from_type_ident(&ident))
                    {
                        let bounds = bounds
                            .into_iter()
                            .filter_map(|b| {
                                {
                                    if let syn::TypeParamBound::Lifetime(l) = &b
                                    {
                                        if struct_generics.contains(&(l).into())
                                        {
                                            Some(0)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }.map(move |_| b)
                            })
                            .collect();
                        Some(syn::GenericParam::Type(syn::TypeParam {
                            ident,
                            colon_token,
                            bounds,
                            attrs: vec![],
                            eq_token: None,
                            default: None,
                        }))
                    } else {
                        None
                    }
                },
                _ => None,
            }
        })
        .collect::<Punctuated<_, _>>();
    let span = params.span();
    let generics = syn::Generics {
        params,
        lt_token: Some(span.into()),
        gt_token: Some(span.into()),
        where_clause,
    };
    generics
}



impl<'a> AsGenerics<'a> for syn::Variant {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> { self.fields.as_generic() }
}

impl<'a> AsGenerics<'a> for syn::Fields {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
            | syn::Fields::Unnamed(syn::FieldsUnnamed {
                unnamed: fields,
                ..
            }) => fields.iter().flat_map(|f| f.as_generic()).collect(),
            syn::Fields::Unit => vec![],
        }
    }
}

impl<'a> AsGenerics<'a> for syn::Field {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> { self.ty.as_generic() }
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
            Slice(syn::TypeSlice { elem: t, .. })
            | Array(syn::TypeArray { elem: t, .. })
            | Ptr(syn::TypePtr { elem: t, .. })
            | Paren(syn::TypeParen { elem: t, .. })
            | Group(syn::TypeGroup { elem: t, .. }) => t.as_generic(),
            Tuple(syn::TypeTuple { elems: ts, .. }) => {
                ts.iter().flat_map(|t| t.as_generic()).collect()
            },
            Reference(syn::TypeReference {
                lifetime, elem: t, ..
            }) => {
                let mut out = lifetime.as_generic();
                out.append(&mut t.as_generic());
                out
            },
            Path(syn::TypePath { path: p, .. }) => p.as_generic(),
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
    fn new_from_type_ident(id: &'a syn::Ident) -> Self { GenericType::Type(id) }
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
    fn as_generic(&'a self) -> Vec<GenericType<'a>> { vec![*self] }
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



#[allow(dead_code)]
#[cfg(test)]
mod tests {
    use super::{
        AsGenerics,
        GenericType,
        *,
    };

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

    #[test]
    fn test_as_type() {
        let ident = syn::Ident::new("TraitName", Span::call_site());
        let trait_ = as_type_trait(&syn::Visibility::Inherited, &ident);
        println!("\n---\n{}\n---\n", quote!(#trait_));
        let parsed = syn::parse2::<syn::Item>(quote!(#trait_)).unwrap();
        println!("{:?}", parsed);
        assert_eq!(parsed, trait_);
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
