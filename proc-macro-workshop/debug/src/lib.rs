use proc_macro::TokenStream;
use syn::DeriveInput;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as DeriveInput);

    match do_syntax_tree_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn do_syntax_tree_expand(_st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let new_token_stream = proc_macro2::TokenStream::new();

    Ok(new_token_stream)
}
