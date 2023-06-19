use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, AttributeArgs, Item};

/// function name is <b>the marco name</b>.
#[proc_macro_attribute]
pub fn hello_world_proc_marco(attr: TokenStream, item: TokenStream) -> TokenStream {
    eprintln!("{:#?}", parse_macro_input!(attr as AttributeArgs));

    let body_ast = parse_macro_input!(item as Item);
    eprintln!("{:#?}", body_ast);

    quote!(#body_ast).into()
}
