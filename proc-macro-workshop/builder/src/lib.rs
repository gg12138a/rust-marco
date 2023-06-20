use proc_macro::TokenStream;

use syn;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let syntax_tree = syn::parse_macro_input!(input as syn::DeriveInput);
    
    eprintln!("{:#?}", syntax_tree);

    TokenStream::new()
}
