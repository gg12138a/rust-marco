//! Example use for derive macro `Builder`:
//!
//! ```ignore
//! #[derive(Builder)]
//! pub struct Command {
//!     executable: String,
//!     args: Vec<String>,
//!     env: Vec<String>,
//!     current_dir: Option<String>,
//! }
//! ```
//!
//! then code below will be append:
//!
//! ```ignore
//! pub struct CommandBuilder {
//!     executable: Option<String>,
//!     args: Option<Vec<String>>,
//!     env: Option<Vec<String>>,
//!     current_dir: Option<String>,
//! }
//!
//! impl Command {
//!     pub fn builder() -> CommandBuilder {
//!         CommandBuilder {
//!             executable: None,
//!             args: None,
//!             env: None,
//!             current_dir: None,
//!         }
//!     }
//! }
//!
//! impl CommandBuilder {
//!     pub fn executable(&mut self, executable: String) -> &mut Self {
//!         self.executable = Some(executable);
//!         self
//!     }
//!
//!     // ...(other setters)
//!
//!     pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
//!         // check all fields not none
//!     }
//! }
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{self, spanned::Spanned, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let syntax_tree = syn::parse_macro_input!(input as syn::DeriveInput);

    match do_st_expand(&syntax_tree) {
        Ok(marco2_token_stream) => marco2_token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_st_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let primitive_struct_name_literal = st.ident.to_string();
    let builder_struct_name_literal = format!("{}Builder", primitive_struct_name_literal);

    let builder_struct_ident = syn::Ident::new(&builder_struct_name_literal, st.span());
    let primitive_struct_ident = &st.ident;

    let builder_struct_fields_def = generate_builder_struct_fields_def(st).unwrap();
    let init_clauses = generate_builder_struct_factory_fn_init_clauses(st).unwrap();
    let setters = generate_setters_for_builder_struct(st).unwrap();
    let build_fn = generate_build_fn_for_builder_struct(st).unwrap();

    let macro2_token_stream = quote!(
        pub struct #builder_struct_ident {
            #builder_struct_fields_def
        }

        impl #primitive_struct_ident {
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

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields(st: &DeriveInput) -> syn::Result<&StructFields> {
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
fn generate_builder_struct_fields_def(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields(st).unwrap();

    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let tys: Vec<_> = fields
        .iter()
        .map(|f| {
            // if original field type R eqauls `Option<T>`, the corresponding field type in `XXXBuilder`
            // will be type `Option<T>` or else type `Option<R>`.
            if let Some(generic_argument_ty) =
                identify_option_type_token_stream_in_original_struct_def(&f.ty)
            {
                quote!(std::option::Option<#generic_argument_ty>)
            } else {
                let original_ty = &f.ty;
                quote!(std::option::Option<#original_ty>)
            }
        })
        .collect();

    Ok(quote!(
        #(#idents: #tys),*
    ))
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
    st: &DeriveInput,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let fields = get_fields(st).unwrap();

    Ok(fields
        .iter()
        .map(|f| {
            let ident = &f.ident;

            quote!(
                #ident : std::option::Option::None
            )
        })
        .collect())
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
fn generate_setters_for_builder_struct(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields(st).unwrap();

    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let tys: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut to_append_tokenstream = proc_macro2::TokenStream::new();
    for (ident, ty) in idents.iter().zip(tys.iter()) {
        // ty will be type `T` if original type `R` equals `Option<T>` otherwise type `R`.
        let ty = if let Some(ty) = identify_option_type_token_stream_in_original_struct_def(&ty) {
            ty
        } else {
            ty
        };

        let setter_tokenstream = quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self{
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        );

        to_append_tokenstream.extend(setter_tokenstream);
    }

    Ok(to_append_tokenstream)
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
fn generate_build_fn_for_builder_struct(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields(st).unwrap();

    // check must not be None fields.
    let mut check_all_fields_not_none_code = Vec::new();
    for field in fields {
        let ident = &field.ident;

        // skip fields with type `Option<T>` in original struct def.
        if let None = identify_option_type_token_stream_in_original_struct_def(&field.ty) {
            check_all_fields_not_none_code.push(quote!(
                if self.#ident.is_none() {
                    let err = format!("field {} still none, can't build now.", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            ))
        }
    }

    // field will be None if not set when fn `build` called.
    let mut to_fill_result_variable_clauses = Vec::new();
    for field in fields {
        let ident = &field.ident;

        if let None = identify_option_type_token_stream_in_original_struct_def(&field.ty) {
            // original type is not `Option<T>`, so we need unwrap `Option<T>` to `T`.
            to_fill_result_variable_clauses.push(quote!(
                #ident: self.#ident.clone().unwrap()
            ));
        } else {
            to_fill_result_variable_clauses.push(quote!(
                #ident: self.#ident.clone()
            ));
        }
    }

    let original_struct_ident = &st.ident;
    let build_fn = quote!(
        pub fn build(&self) -> std::result::Result<#original_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#check_all_fields_not_none_code)*

            std::result::Result::Ok(#original_struct_ident {
                #(#to_fill_result_variable_clauses),*
            })
        }
    );

    Ok(build_fn)
}

/// return type `T` in type `Option<T>`.
fn identify_option_type_token_stream_in_original_struct_def(ty: &syn::Type) -> Option<&syn::Type> {
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
