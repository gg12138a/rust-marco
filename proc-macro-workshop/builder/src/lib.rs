use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{self, spanned::Spanned, DeriveInput, Field};

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

/// Wrapper for type `StructFields` with more metadata used when marco processing.
struct StructFieldsWithMetadata<'a> {
    /// original field literal.
    ident: &'a Ident,
    /// original field type.
    original_ty: &'a syn::Type,
    /// type T in Option<T>.
    inner_ty_in_option: Option<&'a syn::Type>,
    /// type T in Vec<T>.
    inner_ty_meta_in_vec_type: Option<VecTypeMeta<'a>>,
}

struct VecTypeMeta<'a> {
    /// type T in Vec<T>.
    inner_argument_ty: &'a syn::Type,
    /// value of the key "each" in attribute `builder`.
    val_of_the_key_each: Option<Ident>,
}

impl<'a> StructFieldsWithMetadata<'a> {
    /// check if the original type in struct def equals `Option<T>`.
    pub fn is_original_type_equals_option(&self) -> bool {
        return self.inner_ty_in_option.is_some();
    }

    pub fn is_original_type_equals_vec_and_carry_valid_attribute(&self) -> bool {
        if let Some(VecTypeMeta {
            val_of_the_key_each: Some(_),
            ..
        }) = self.inner_ty_meta_in_vec_type
        {
            true
        } else {
            false
        }
    }
}

fn get_fields_from_syntax_tree(st: &DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(st, "could only defined in struct."))
    }
}

/// Example use for derive macro `Builder`:
///
/// ```ignore
/// #[derive(Builder)]
/// pub struct Command {
///     executable: String,
///     args: Vec<String>,
///     env: Vec<String>,
///     current_dir: Option<String>,
/// }
/// ```
///
/// then code below will be append:
///
/// ```ignore
/// pub struct CommandBuilder {
///     executable: Option<String>,
///     args: Option<Vec<String>>,
///     env: Option<Vec<String>>,
///     current_dir: Option<String>,
/// }
///
/// impl Command {
///     pub fn builder() -> CommandBuilder {
///         CommandBuilder {
///             executable: None,
///             args: None,
///             env: None,
///             current_dir: None,
///         }
///     }
/// }
///
/// impl CommandBuilder {
///     pub fn executable(&mut self, executable: String) -> &mut Self {
///         self.executable = Some(executable);
///         self
///     }
///
///     // ...(other setters)
///
///     pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
///         // check all fields not none
///     }
/// }
/// ```
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let syntax_tree = syn::parse_macro_input!(input as syn::DeriveInput);

    match do_st_expand(&syntax_tree) {
        Ok(marco2_token_stream) => marco2_token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// parse the token stream of original struct def to syntax tree,
/// and then convert it to type `proc_marco2::TokenStream` after append the corresponding builder struct def.
fn do_st_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let original_struct_name_literal = st.ident.to_string();
    let builder_struct_name_literal = format!("{}Builder", original_struct_name_literal);

    let original_struct_ident = &st.ident;
    let builder_struct_ident = Ident::new(&builder_struct_name_literal, st.span());

    let fields = collect_fields_metadata_from_syntax_tree(st)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(&fields);
    let init_clauses = generate_builder_struct_factory_fn_init_clauses(&fields);
    let setters = generate_setters_for_builder_struct(&fields)?;
    let build_fn = generate_build_fn_for_builder_struct(original_struct_ident, &fields);

    let macro2_token_stream = quote!(
        pub struct #builder_struct_ident {
            #builder_struct_fields_def
        }

        impl #original_struct_ident {
            pub fn builder() -> #builder_struct_ident {
                #builder_struct_ident {
                    #(#init_clauses),*
                }
            }
        }

        impl #builder_struct_ident {
            #setters

            #build_fn
        }
    );

    Ok(macro2_token_stream)
}

/// process every field in original struct definition.
fn collect_fields_metadata_from_syntax_tree(
    st: &DeriveInput,
) -> syn::Result<Vec<StructFieldsWithMetadata>> {
    let fields_without_medata = get_fields_from_syntax_tree(st).unwrap();

    let mut v = vec![];
    for field in fields_without_medata {
            let ident = &field.ident.as_ref().unwrap();
            let original_ty = &field.ty;
            let inner_ty_in_option = extract_inner_type_of_option(&field.ty);

            let inner_ty_meta_in_vec_type =
                if let Some(ty_in_vec_type) = extract_inner_type_of_vec(&field.ty) {
                    let attr_val = value_of_the_key_each_in_attribute_builder(&field)?;
                    Some(VecTypeMeta {
                        inner_argument_ty: ty_in_vec_type,
                        val_of_the_key_each: attr_val,
                    })
                } else {
                    None
                };

            v.push(StructFieldsWithMetadata {
                ident,
                original_ty,
                inner_ty_in_option,
                inner_ty_meta_in_vec_type,
            });
        }
    
    Ok(v)
}

/// expand syntax tree to get XXXBuilder struct definition.
///
/// for type T of field defined in original struct, the corresponding field type in XXXBuilder is:
/// - `Option<T>` -> `Option<T>`, instead of `Option<Option<T>>`.
/// - `T` -> `T`
///
/// # Expand Result Example
/// ```ignore
/// // pub struct CommandBuilder {
///         executable: Option<String>,     // original type `String`
///         args: Option<Vec<String>>,
///         env: Option<Vec<String>>,
///         current_dir: Option<String>,    // original type `Option<String>`, now we will get the right type `Option<String>` instead of type `Option<Option<String>>`
/// // }
///```
fn generate_builder_struct_fields_def(
    fields: &Vec<StructFieldsWithMetadata>,
) -> proc_macro2::TokenStream {
    let (idents, tys): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|field| {
            let ty = if let Some(VecTypeMeta {
                val_of_the_key_each: Some(_),
                ..
            }) = field.inner_ty_meta_in_vec_type
            {
                // field type will be `Vec<T>` if original type is `Vec<T>` with "each" attribute set.
                let ty = field.original_ty;
                quote!(#ty)
            } else if let Some(type_in_option) = field.inner_ty_in_option {
                // field type will be `Option<T>` if original type is `Option<T>`.
                quote!(std::option::Option<#type_in_option>)
            } else {
                // field type will be `Option<T>` if original type is `T`.
                // contains this scenario: original type is `Vec<T>`, but no attribute parsed, then
                // the type will be Option<Vec<T>>.
                let ty = field.original_ty;
                quote!(std::option::Option<#ty>)
            };

            (field.ident, ty)
        })
        .unzip();

    quote!(
        #(#idents: #tys),*
    )
}

/// expand syntax tree to get impl block for `XXX` struct.
///
/// # Expand Result Example
///
/// ```ignore
/// // impl Command {
/// //     pub fn builder() -> CommandBuilder {
/// //          CommandBuilder {
///                 executable: None,
///                 args: None,
///                 env: None,
///                 current_dir: None,
/// //         }
/// //     }
/// // }
/// ```
fn generate_builder_struct_factory_fn_init_clauses(
    fields: &Vec<StructFieldsWithMetadata>,
) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|field_meta| {
            let ident = field_meta.ident;
            if let Some(VecTypeMeta {
                val_of_the_key_each: Some(_),
                ..
            }) = field_meta.inner_ty_meta_in_vec_type
            {
                // field type will be `Vec<T>` if original type is `Vec<T>` with "each" attribute set.
                quote!(#ident: std::vec::Vec::new())
            } else {
                quote!(#ident: std::option::Option::None)
            }
        })
        .collect()
}

/// generate setters for `XXXBuilder` struct.
///
/// original type in XXX struct def, and the type in corresponding setter fn:
/// - T -> T
/// - Option<T> -> T
/// - Vec<T>, one of below:
///     - Vec<T> if the field has attribute `each` -> T, the setter fn name is the ident in attribute.
///     - Vec<T> -> Vec<T>, the setter fn name is the field name.
///
/// # Expand Result Example(code that not commented)
///
/// ```ignore
/// // impl CommandBuilder {
///     fn executable(&mut self, executable: String) -> &mut Self {
///         self.executable = Some(executable);
///         self
///     }
///
///     // other fields ...
/// // }
/// ```
fn generate_setters_for_builder_struct(
    fields: &Vec<StructFieldsWithMetadata>,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut setters = proc_macro2::TokenStream::new();

    let mut setters_fn_name_literal_vec: Vec<_> = vec![];
    for field in fields {
        let ident = field.ident;
        if let Some(VecTypeMeta {
            val_of_the_key_each: Some(attr_val),
            inner_argument_ty,
        }) = &field.inner_ty_meta_in_vec_type
        {
            // check duplicate literal
            if setters_fn_name_literal_vec.contains(&attr_val.to_string()) {
                return Err(syn::Error::new_spanned(
                    attr_val,
                    "duplicate setter name found!",
                ));
            }

            // fn name: attr_val
            // param ty: inner_argument_ty, T in Vec<T>.
            setters.extend(quote!(
                  fn #attr_val(&mut self, #attr_val: #inner_argument_ty) -> &mut Self {
                        self.#ident.push(#attr_val);
                        self
                    }
            ));
            setters_fn_name_literal_vec.push(attr_val.to_string());
        } else if let Some(inner_ty_in_option) = field.inner_ty_in_option {
            // fn name: field name
            // param ty: inner_ty_in_option, T in Option<T>.
            setters.extend(quote!(
                fn #ident(&mut self, #ident: #inner_ty_in_option) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            ));

            setters_fn_name_literal_vec.push(ident.to_string());
        } else {
            // fn name: field name
            // param ty: T

            let ty = field.original_ty;
            setters.extend(quote!(
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            ));

            setters_fn_name_literal_vec.push(ident.to_string());
        };
    }

    Ok(setters)
}

/// generate build fn for `XXXBuilder` struct.
///
///  # Expand Result Example
///
/// ```ignore
/// impl CommandBuilder {
///     pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
///         ...
///     }
/// }
/// ```
fn generate_build_fn_for_builder_struct(
    original_struct_ident: &Ident,
    fields: &Vec<StructFieldsWithMetadata>,
) -> proc_macro2::TokenStream {
    let mut code_pieces_of_check_all_must_set_fields = Vec::new();
    for field in fields {
        // skip fields with type `Option<T>` in original struct def.
        if !field.is_original_type_equals_option()
            && !field.is_original_type_equals_vec_and_carry_valid_attribute()
        {
            let ident = field.ident;
            code_pieces_of_check_all_must_set_fields.push(quote!(
                if self.#ident.is_none() {
                    let err = format!("field {} still none, can't build now.", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            ));
        }
    }

    // def of fn `build`.
    let mut to_fill_original_struct_clauses = Vec::new();
    for field in fields {
        let ident = field.ident;

        if field.is_original_type_equals_option() {
            to_fill_original_struct_clauses.push(quote!(
                #ident: self.#ident.clone()
            ));
        } else if field.is_original_type_equals_vec_and_carry_valid_attribute() {
            to_fill_original_struct_clauses.push(quote!(
                #ident: self.#ident.clone()
            ))
        } else {
            to_fill_original_struct_clauses.push(quote!(
                #ident: self.#ident.clone().unwrap()
            ));
        }
    }

    quote!(
        pub fn build(&self) -> std::result::Result<#original_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#code_pieces_of_check_all_must_set_fields)*

            std::result::Result::Ok(#original_struct_ident {
                #(#to_fill_original_struct_clauses),*
            })
        }
    )
}

/// return type `T` in type `Option<T>`.
///
/// for type `std::option::Option<T>`, the corresponding token stream structure is:
/// ```ignore
/// Type::Path(
///     TypePath {
///         qself: None,
///         path: Path {
///             segments: [
///                 PathSegment {
///                     ident: "Option",
///                     arguments: PathArguments::AngleBracketed(
///                         AngleBracketedGenericArguments {
///                             args: [
///                                 GenericArgument::Type(
///                                     ...
///                                 ),
///                             ],
///                         },
///                     ),
///                 },
///             ],
///         },
///     },
/// )
/// ```
///
/// > Tip: the general type def is `org::example::Foo<T,R,U>`.
///
/// TODO: refactor this fn to generic. the input and corresponding output should be:
/// - T -> None
/// - Option<T> -> Some(T)
/// - Vec<T> -> Some(T)
fn try_extract_generic_type_argument<'a>(
    try_to_extract_type: &'a syn::Type,
    to_extract_outer_type_literal: &'a str,
) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = try_to_extract_type
    {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == to_extract_outer_type_literal {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = &seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(ref ty)) = args.first() {
                        return Some(ty);
                    }
                }
            }
        }
    }

    None
}

/// if param `arbitrary_ty` is Option<T> type, then return Some(T).
fn extract_inner_type_of_option(arbitrary_ty: &syn::Type) -> Option<&syn::Type> {
    try_extract_generic_type_argument(arbitrary_ty, "Option")
}

/// if param `arbitrary_ty` is Vec<T> type, then return Some(T).
fn extract_inner_type_of_vec(arbitrary_ty: &syn::Type) -> Option<&syn::Type> {
    try_extract_generic_type_argument(arbitrary_ty, "Vec")
}

/// parse the value corresponding the key "each" in attribute `builder`.
///
/// # Example
///
/// ```ignore
/// #[Builder]
/// struct Foo{
///     #[builder(each = "arg")]
///     args: Vec<T>
/// }
/// ```
fn value_of_the_key_each_in_attribute_builder(field: &Field) -> syn::Result<Option<syn::Ident>> {
    // find the attribute `builder`

    for attr in &field.attrs {
        if let syn::Result::Ok(syn::Meta::List(list)) = attr.parse_meta() {
            let syn::MetaList {
                ref path,
                ref nested,
                ..
            } = list;

            if let Some(path_seg) = path.segments.first() {
                if path_seg.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv_entry))) =
                        nested.first()
                    {
                        if kv_entry.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv_entry.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            return Err(syn::Error::new_spanned(
                                list,
                                r#"expected `builder(each = "...")`"#,
                            ));
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}
