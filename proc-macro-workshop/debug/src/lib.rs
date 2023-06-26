use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::DeriveInput;

/// # Example
///
/// ```ignore
/// #[derive(CustomDebug)]
/// pub struct Field {
///     name: &'static str,
///     bitmask: u8,
/// }
/// ```
#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as DeriveInput);

    match do_syntax_tree_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn do_syntax_tree_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let original_struct_ident = &st.ident;
    let fields = get_fields_from_struct_def_st(st)?;

    Ok(generate_debug_trait_impl_block(
        &original_struct_ident,
        &fields,
    )?)
}

fn get_fields_from_struct_def_st(st: &DeriveInput) -> syn::Result<Vec<&syn::Field>> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = &st.data
    {
        Ok(named.iter().collect())
    } else {
        Err(syn::Error::new_spanned(
            st,
            r#"the proc macro "CustomDebug" could only defined in strcut."#,
        ))
    }
}

/// # Example
///
/// ```ignore
/// impl std::fmt::Debug for Field {
///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///         f.debug_struct("Field")
///             .field("name", &self.name)
///             .field("bitmask", &self.bitmask)
///             .finish()
///     }
///  }
/// ```
fn generate_debug_trait_impl_block(
    original_struct_ident: &Ident,
    fields: &Vec<&syn::Field>,
) -> syn::Result<proc_macro2::TokenStream> {
    let original_struct_literal = original_struct_ident.to_string();

    let mut fmt_fn_body = proc_macro2::TokenStream::new();
    fmt_fn_body.extend(quote!(f.debug_struct(#original_struct_literal)));
    for field in fields {
        let field_ident = &field.ident.as_ref().unwrap();
        let field_literal = field_ident.to_string();

        fmt_fn_body.extend(quote!(.field(#field_literal, &self.#field_ident)));
    }
    fmt_fn_body.extend(quote!(.finish()));

    let impl_block_token_stream = quote!(
        impl std::fmt::Debug for #original_struct_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt_fn_body
            }
        }
    );

    Ok(impl_block_token_stream)
}
