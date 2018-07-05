#![feature(rust_2018_edition, rustc_private, proc_macro, extern_prelude,
           iterator_flatten)]
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
use quote::{
    pounded_var_names,
    quote,
    quote_each_token,
    quote_spanned,
};
use std::{
    ops::Deref,
    vec::IntoIter,
};
use syn::{
    punctuated::Punctuated,
    spanned::Spanned,
    Data,
    DeriveInput,
};

#[proc_macro_derive(DefaultEnum, attributes(Default))]
pub fn default_enum(input: TokenStream) -> TokenStream {
    // panic!("{:?}", input);

    let input: proc_macro2::TokenStream = input.into();

    let ast: DeriveInput = syn::parse2(input).unwrap();

    // panic!("{:?}", quote!(#ast));

    // let vis = &ast.vis;
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

    let default_variant =
        default_variants_matched.next()
                                .expect("At least one variant must be marked \
                                         as #[Default]");

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
            (quote!({#(#enum_fields,)*}), quote!(where #(#types : Default,)*))
        },
        Fields::Unnamed(ref fs) => {
            let enum_fields =
                fs.unnamed.iter().map(|e| {
                                          let ty = &e.ty;
                                          quote!(<#ty as Default> :: default())
                                      });
            let types = fs.unnamed.iter().map(|e| &e.ty);
            (quote!((#(#enum_fields,)*)), quote!(where #(#types : Default,)*))
        },
    };

    let enum_name = &ast.ident;

    let here_default = quote!(Default);
    // let span = enum_name.span();

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

    // panic!("{}", out);

    out.into()
}


#[proc_macro_attribute]
pub fn enum_inner(_: TokenStream, input: TokenStream) -> TokenStream {
    // panic!("\n\n{}\n{:?}", input, input);
    let input: proc_macro2::TokenStream = input.into();
    // panic!("\n\n{}\n{:?}", input, input);
    let ast: DeriveInput = syn::parse2(input).unwrap();
    // panic!("\n{:?}", quote_spanned!(ast.span()=> #ast));
    let name = &ast.ident;
    let vis = &ast.vis;
    // panic!("{:?}", input);
    let enum_data = ensure_enum(&ast);

    // let reduced_bounds = enum_data
    //     .variants
    //     .iter()
    //     .map(|v| reduce_bounds(&ast, v))
    //     .map(|b| {
    //         let (_, ty, wh) = b.split_for_impl();
    //         format!(
    //             "{}\n",
    //             quote!{
    //                 #vis struct #ty {}
    //                 #wh
    //             }
    //         )
    //     });

    // panic!("\n{}", reduced_bounds.collect::<String>());

    let var_names = variant_names(enum_data).into_iter().collect::<Vec<_>>();
    let var_names_iter = var_names.iter();
    // let out = quote!(#ast).into();
    let id_name: syn::Ident =
        syn::Ident::new(&format!("{}Type", name).to_camel_case(), name.span());
    let id_enum = quote_spanned!{name.span()=>
        #[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
        #vis enum #id_name { #(#var_names_iter,)* }
    };
    let (impl_generics, ty_generics, where_clause) =
        ast.generics.split_for_impl();
    let orig_and_id_variants = enum_data.variants.iter().zip(var_names.iter());
    let match_arms = orig_and_id_variants.map(|(orig, id)| {});
    let trait_name_string = format!("As{}", id_name);
    let trait_name =
        syn::Ident::new(&trait_name_string.to_camel_case(), name.span());
    let fn_name =
        syn::Ident::new(&trait_name_string.to_snake_case(), name.span());
    let as_trait = quote_spanned!{name.span()=>
        #vis trait #trait_name {
            fn #fn_name(&self) -> #id_name;
        }
    };
    // panic!("{:?}", as_trait);
    let trait_impl = var_names.iter().map(|v| {
        quote_spanned!{v.span()=>
            impl #impl_generics #trait_name for #v #ty_generics
            #where_clause
            {
                fn #fn_name(&self) -> #id_name {
                    #id_name :: #v
                }
            }
        }
    });

    let variant_types = enum_data.variants.iter().map(|v| {
        let lifetime_bounds = ast.generics.lifetimes().into_iter().map(|l| l.lifetime.clone()).collect::<Punctuated<_,Token![,]>>();
        let (lifes, tys) = GenericType::lifetimes_and_types(v.as_generic());
        let lifes = lifetime_bounds_intersection(&lifetime_bounds, &lifes);
        // let lifes = lifes.into_iter().map(|g| g.as_tokens());
        let tys = tys.into_iter().map(|g| g.as_tokens());
        quote!(<#(#lifes,)* #(#tys),*>)
    });

    // panic!("{}", quote!(#(#variant_types),*));

    let inner_structs = enum_data
        .variants
        .iter()
        .map(|v| {
            use syn::Fields::*;
            let punc = match v.fields {
                Named(_) => quote!(),
                Unnamed(_) => quote_spanned!(v.span()=> ;),
                Unit => quote_spanned!(v.span()=> {}),
            };
            let bounds = reduce_bounds(&ast, v);
            let (impl_types, struct_types, struct_where) = bounds.split_for_impl();
            // let struct_generics = reduce_generics(&ast, v);
            let attrs = &v.attrs;
            let ident = &v.ident;
            let fields = &v.fields;

            quote_spanned!{ v.span()=>
                #(#attrs)*
                #vis struct #ident #impl_types
                #struct_where
                #fields #punc

                impl #impl_types #trait_name for #ident #struct_types
                #struct_where
                {
                    fn #fn_name(&self) -> #id_name {
                        #id_name :: # ident
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    let inner_struct_iter = inner_structs.iter();
    let out = quote_spanned!{ast.span()=>
        #id_enum
        // #ast
        #(#inner_struct_iter)*
        #as_trait
        // #(#trait_impl)*
    };
    // panic!("{}", out);
    // let inner_struct_iter = inner_structs.iter();
    // let in_struct = quote!(#(#inner_struct_iter)*);
    // return in_struct.into();
    // panic!("\n\n{}\n{:?}", out, out);
    out.into()
}

fn generic_types(ast: &DeriveInput) -> impl IntoIterator<Item = &syn::Ident> {
    ast.generics.type_params().map(|p| &p.ident)
}

fn inner_types(ast: &syn::Variant) -> impl IntoIterator<Item = &syn::Type> {
    use syn::{
        Fields::*,
        FieldsNamed,
        FieldsUnnamed,
    };
    match ast.fields {
        Named(FieldsNamed {
            brace_token: _,
            named: ref punc,
        })
        | Unnamed(FieldsUnnamed {
            paren_token: _,
            unnamed: ref punc,
        }) => Some(punc.iter().map(|f| &f.ty)),
        _ => None,
    }.into_iter()
        .flatten()
        .flat_map(|t| {
            use syn::Type::*;
            match t {
                Slice(syn::TypeSlice { elem: ty, .. })
                | Ptr(syn::TypePtr { elem: ty, .. })
                | Reference(syn::TypeReference { elem: ty, .. })
                | Array(syn::TypeArray { elem: ty, .. }) => Some(ty.deref()),
                Tuple(_) => unimplemented!(), /* TODO Deal with tuples in inner structs */
                _ => panic!(),
            }
            // Some(t)
        })
}
// fn inner_types(ast: &syn::Variant) -> impl IntoIterator<Item = &syn::Type> {

fn generic_lifetimes(ast: &DeriveInput)
                     -> impl IntoIterator<Item = &syn::Lifetime> {
    ast.generics.lifetimes().map(|l| &l.lifetime)
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

fn type_paths(ty: &syn::Type) -> Vec<&syn::Path> {
    use syn::Type::*;
    match ty {
        Slice(syn::TypeSlice { elem: t, .. })
        | Array(syn::TypeArray { elem: t, .. })
        | Ptr(syn::TypePtr { elem: t, .. })
        | Reference(syn::TypeReference { elem: t, .. })
        | Paren(syn::TypeParen { elem: t, .. })
        | Group(syn::TypeGroup { elem: t, .. }) => type_paths(t),
        Tuple(syn::TypeTuple { elems: ts, .. }) => {
            ts.iter().flat_map(|t| type_paths(t)).collect()
        },
        Path(syn::TypePath { path: p, .. }) => vec![p],
        Never(_) | Macro(_) | Verbatim(_) | Infer(_) => vec![],
        BareFn(_) | TraitObject(_) | ImplTrait(_) => vec![],
    }
}

// fn used_generic_type_idents(path: &syn::Path) -> Vec<&syn::Ident> {
// }

// fn final_type_idents_from_path_segment(seg: &syn::PathSegment) -> (&syn::Ident, Vec<&syn::Ident>) {
//     let out =
//     match seg.arguments {
//         syn::PathArguments::AngleBracketed(ref args) => {
//             args.args.iter().map(|ga| {
//                 match ga {
//                     syn::GenericArgument::Lifetime(l) => {
//                     }
//                 }
//             });
//             vec![]
//         }
//         syn::PathArguments::Parenthesized(ref args) => {
//             vec![]
//         }
//         syn::PathArguments::None => vec![]
//     };
//     (&seg.ident, out)
// }


fn lifetime_bounds_intersection<P: Default>(
    lifetime_bounds: &'a syn::punctuated::Punctuated<syn::Lifetime, P>,
    lifetimes: &Vec<GenericType>)
    -> syn::punctuated::Punctuated<&'a syn::Lifetime, P> {
    // lifetime_bounds.iter().filter(|&l| lifetimes.contains(&l.into())).collect()
    lifetime_bounds.strip(lifetimes)
}

trait Strip<'a, T: 'a, P: Default>: IntoIterator<Item = &'a T> {
    fn strip(self,
             keep: &Vec<GenericType>)
             -> syn::punctuated::Punctuated<&'a T, P>;
}

impl<'a, T: 'a, P: Default> Strip<'a, T, P> for &'a Punctuated<T, P>
    where &'a T: Into<GenericType<'a>>
{
    fn strip(self, keep: &Vec<GenericType>) -> Punctuated<&'a T, P> {
        self.into_iter().filter(|&t| keep.contains(&t.into()))
            .collect()
    }
}

// fn normalize_where_predicates(clause: syn::WhereClause) -> Option<syn::WhereClause> {
//     if let Some(_) =
// }

fn reduce_bounds(enum_ast: &DeriveInput,
                 struct_ast: &syn::Variant)
                 -> syn::Generics {
    // Punctuated<syn::GenericParam, Token![,]> {
    let enum_generics = enum_ast.generics.as_generic();
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
                        let bounds = bounds
                            .strip(&struct_generics)
                            .into_iter()
                            .cloned()
                            .collect();
                        Some(syn::WherePredicate::Lifetime(
                            syn::PredicateLifetime {
                                lifetime,
                                colon_token,
                                bounds,
                            },
                        ))
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
                    // let ident = syn::Ident::new(
                    //     "placeholder",
                    //     proc_macro2::Span::call_site(),
                    // );
                    let relevant = {
                        let bounded_types = bounded_ty.as_generic();
                        bounded_types.len() > 0
                            && bounded_types
                                .iter()
                                .all(|t| struct_generics.contains(t))
                    };

                    if relevant {
                        // }
                        // if struct_generics
                        //     .contains(&GenericType::new_from_type_ident(&ident))
                        // {
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
    let where_clause = Some(syn::WhereClause { where_token: syn::token::Where::default(),
                                predicates:  reduced_where_bounds.collect(), });
    let params = enum_ast.generics
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
                             // None
                         })
                         .collect::<Punctuated<_, _>>();
    let span = params.span();
    let generics = syn::Generics { params,
                                   lt_token: Some(span.into()),
                                   gt_token: Some(span.into()),
                                   where_clause, };
    generics
    // let reduced = intersection(enum_generics.iter(), struct_generics.iter())
    //     .into_iter()
    //     .map(|g| g.as_tokens());

    // quote!(<#(#reduced),*>)

    // let attrs = &struct_ast.attrs;
    // let ident = &struct_ast.ident;
    // let fields = &struct_ast.fields;
    // let semicolon = if let syn::Fields::Named(_) = fields {
    //     quote!(;)
    // } else {
    //     quote!()
    // };

    // quote!{
    //     #(#attrs)*
    //     struct #ident <#(#reduced),*>
    //         #fields #semicolon
    // }
}


fn reduce_generics(enum_ast: &DeriveInput,
                   struct_ast: &syn::Variant)
                   -> proc_macro2::TokenStream {
    let enum_generics = enum_ast.generics.as_generic();
    let struct_generics = struct_ast.as_generic();
    let reduced = intersection(enum_generics.iter(), struct_generics.iter())
        .into_iter()
        .map(|g| g.as_tokens());

    quote_spanned!(struct_ast.span()=> <#(#reduced),*>)

    // let attrs = &struct_ast.attrs;
    // let ident = &struct_ast.ident;
    // let fields = &struct_ast.fields;
    // let semicolon = if let syn::Fields::Named(_) = fields {
    //     quote!(;)
    // } else {
    //     quote!()
    // };

    // quote!{
    //     #(#attrs)*
    //     struct #ident <#(#reduced),*>
    //         #fields #semicolon
    // }
}



impl<'a> AsGenerics<'a> for syn::Variant {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> { self.fields.as_generic() }
}

impl<'a> AsGenerics<'a> for syn::Fields {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
            | syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed:
                                                          fields,
                                                      .. }) => {
                fields.iter().flat_map(|f| f.as_generic()).collect()
            },
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
        vec![self::GenericType::new_from_lifetime_ident(&self.lifetime.ident)]
    }
}

impl<'a> AsGenerics<'a> for syn::Type {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        use syn::Type::*;
        // println!("AsGenerics");
        match self {
            Slice(syn::TypeSlice { elem: t, .. })
            | Array(syn::TypeArray { elem: t, .. })
            | Ptr(syn::TypePtr { elem: t, .. })
            | Paren(syn::TypeParen { elem: t, .. })
            | Group(syn::TypeGroup { elem: t, .. }) => t.as_generic(),
            Tuple(syn::TypeTuple { elems: ts, .. }) => {
                ts.iter().flat_map(|t| t.as_generic()).collect()
            },
            Reference(syn::TypeReference { lifetime, elem: t, .. }) => {
                // vec![lifetime.as_generic(),t.as_generic()].into_iter().flatten().collect()
                let mut out = lifetime.as_generic();
                out.append(&mut t.as_generic());
                // println!(
                //     "refrence: {} || lifetimes: {:?}, types: {:?}",
                //     quote!(#lifetime #t),
                //     lifetime.as_generic(),
                //     t.as_generic()
                // );
                // println!("ref debug: {:?}", out);
                out
            },
            Path(syn::TypePath { path: p, .. }) => {
                // let p_gen = p.as_generic();
                // println!("{} -- {:?}", quote!(#p), p_gen);
                p.as_generic()
            },
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
                // println!("segment: {}", quote!(#seg));
                if seg.arguments.is_empty() {
                    if segs.len() > 0 {
                        // println!("Too long");
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
            AngleBracketed(syn::AngleBracketedGenericArguments { args,
                                                                 .. }) => {
                args.iter().flat_map(|a| a.as_generic()).collect()
            },
            Parenthesized(syn::ParenthesizedGenericArguments { inputs,
                                                               output,
                                                               .. }) => {
                inputs.iter()
                      .flat_map(|t| t.as_generic())
                      .chain(match output {
                                 syn::ReturnType::Default => vec![],
                                 syn::ReturnType::Type(_, ty) => {
                                     ty.as_generic()
                                 },
                             })
                      .collect()
            },
        }
        // vec![]
    }
}

impl<'a> AsGenerics<'a> for syn::GenericArgument {
    fn as_generic(&'a self) -> Vec<GenericType<'a>> {
        match self {
            syn::GenericArgument::Lifetime(l) => {
                vec![GenericType::Lifetime(&l.ident)]
            },
            syn::GenericArgument::Type(syn::Type::Path(tp)) => {
                tp.path.segments
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

crate fn intersection<'a, T: 'a + PartialEq>(
    a: impl IntoIterator<Item = &'a T>,
    b: impl IntoIterator<Item = &'a T>)
    -> impl IntoIterator<Item = &'a T> {
    let bs: Vec<_> = b.into_iter().collect();
    a.into_iter().filter(move |e| bs.contains(e))
}

enum DoubleOption<T> {
    Zero,
    One(T),
    Two(T, T),
}

impl<T> DoubleOption<T> {
    fn take(mut self) -> Option<T> {
        use self::DoubleOption::{
            One,
            Two,
            Zero,
        };
        match self {
            Zero => None,
            One(t) => {
                self = Zero;
                Some(t)
            },
            Two(t1, t2) => {
                self = One(t2);
                Some(t1)
            },
        }
    }
}

impl<T> IntoIterator for DoubleOption<T> {
    type IntoIter = IntoIter<T>;
    type Item = T;
    fn into_iter(self) -> Self::IntoIter {
        use self::DoubleOption::*;
        match self {
            Zero => vec![],
            One(t) => vec![t],
            Two(t1, t2) => vec![t1, t2],
        }.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::{
        AsGenerics,
        GenericType,
        *,
    };

    // #[test]
    fn test_types() {
        // let ts: proc_macro2::TokenStream = "pub struct Ty<'a, T: 'a + 'b, 'b: \
        //                                     'a, U>(&'a T, Option<U>, usize, \
        //                                     &'a &'b (&'b T, U));"
        //     .parse()
        //     .unwrap();
        let q = quote!{
            pub struct Ty<'a, 'b: 'a, T: 'a + 'b, U>(&'a T, std::option::Option<U>, usize, &'a &'b (&'b T, U));
        };
        // let ast: DeriveInput = syn::parse2(ts.into()).unwrap();
        let ast: DeriveInput = syn::parse2(q).unwrap();
        let data = &ast.data;
        if let syn::Data::Struct(s) = data {
            if let syn::Fields::Unnamed(u) = &s.fields {
                for ty in u.unnamed.iter().map(|f| &f.ty) {
                    // print!("type: {}: ", quote!(#ty));
                    use syn::Type::*;
                    println!("{}", match ty {
                        Verbatim(_) => "verbatim",
                        Group(_) => "Group",
                        Paren(_) => "Paren",
                        Tuple(_) => "Paren",
                        Path(_) => "Path",
                        Reference(_) => "Reference",
                        TraitObject(_) => "TraitObject",
                        _ => "Other",
                    });
                    let a_lifetime = syn::Ident::new("a", ty.span());
                    let u_type = syn::Ident::new("U", ty.span());
                    let c_lifetime = syn::Ident::new("c", ty.span());
                    let union_test: Vec<GenericType> =
                        vec![GenericType::new_from_lifetime_ident(&a_lifetime),
                             GenericType::new_from_lifetime_ident(&c_lifetime),
                             GenericType::new_from_type_ident(&u_type),];
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
        // panic!("{:?}", quote!(#ast));
    }

    #[test]
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
}
