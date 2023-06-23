//! Example use for derive macro `Builder`:
//!
//! ```
//! #[derive(Builder)]
//! pub struct Command {
//!     executable: String,
//!     args: Vec<String>,
//!     env: Vec<String>,
//!     current_dir: String,
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

    // 为了展示两种实现方式
    let builder_struct_fields_def = generate_builder_struct_fields_def(st).unwrap();
    let init_clauses = generate_builder_struct_factory_fn_init_clauses(st).unwrap();

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
/// ```
/// pub struct CommandBuilder {
///     executable: Option<String>,
///     args: Option<Vec<String>>,
///     env: Option<Vec<String>>,
///     current_dir: Option<String>,
/// }
///```
fn generate_builder_struct_fields_def(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields(st).unwrap();

    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let tys: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    Ok(quote!(
        #(#idents: std::option::Option<#tys>),*
    ))
}

/// expand syntax tree to get impl block for `XXX` struct.
///
/// # Expand Result Example
///
/// ```
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
        .collect()
    )
}
