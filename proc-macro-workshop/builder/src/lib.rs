use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{self, spanned::Spanned, DeriveInput};

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

/// Wrapper for type `StructFields`. So we can know if the original type in struct def equals type `Option<T>`.
struct StructFieldsWithMetadata<'a> {
    ident: &'a Option<Ident>,
    original_ty: &'a syn::Type,
    generic_ty_in_option_type: &'a syn::Type,
}

impl<'a> StructFieldsWithMetadata<'a> {
    /// check if the original type in struct def equals `Option<T>`.
    pub fn is_original_type_equals_option(&self) -> bool {
        return self.original_ty != self.generic_ty_in_option_type;
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
#[proc_macro_derive(Builder)]
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

    let fields = get_fields_with_metadata_from_syntax_tree(st).unwrap();
    let builder_struct_fields_def = generate_builder_struct_fields_def(&fields);
    let init_clauses = generate_builder_struct_factory_fn_init_clauses(&fields);
    let setters = generate_setters_for_builder_struct(&fields);
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
/// get (identifier, original type, type T in original type `Option<T>`, is original type equals `Option<T>`)
fn get_fields_with_metadata_from_syntax_tree(
    st: &DeriveInput,
) -> syn::Result<Vec<StructFieldsWithMetadata>> {
    let fields_without_medata = get_fields_from_syntax_tree(st).unwrap();

    Ok(fields_without_medata
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let original_ty = &field.ty;
            let generic_ty_in_option_type = if let Some(generic_argument_ty) =
                extract_generic_type_argument_in_type_option(&field.ty)
            {
                generic_argument_ty
            } else {
                &field.ty
            };

            // (ident, original_ty, processed_ty, is_original_type_equals_option)
            StructFieldsWithMetadata {
                ident,
                original_ty,
                generic_ty_in_option_type,
            }
        })
        .collect())
}

/// expand syntax tree to get XXXBuilder struct definition.
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
            let ty = if field.is_original_type_equals_option() {
                field.generic_ty_in_option_type
            } else {
                field.original_ty
            };

            (field.ident, quote!(std::option::Option<#ty>))
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
    let fields: Vec<_> = fields.iter().map(|field| field.ident).collect();

    fields
        .iter()
        .map(|ident| {
            quote!(
                #ident : std::option::Option::None
            )
        })
        .collect()
}

/// generate setters for `XXXBuilder` struct.
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
) -> proc_macro2::TokenStream {
    let mut setters = proc_macro2::TokenStream::new();
    for field in fields {
        // ty will be type `T` if original type `R` equals `Option<T>` otherwise type `R`.
        let ty = if field.is_original_type_equals_option() {
            field.generic_ty_in_option_type
        } else {
            field.original_ty
        };
        let ident = field.ident;

        let setter_fn = quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        );

        setters.extend(setter_fn);
    }

    setters
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
        if !field.is_original_type_equals_option() {
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
fn extract_generic_type_argument_in_type_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == "Option" {
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
