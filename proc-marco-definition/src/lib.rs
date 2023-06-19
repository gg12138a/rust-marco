use proc_macro::TokenStream;

/// function name is <b>the marco name</b>.
#[proc_macro_attribute]
pub fn hello_world_proc_marco(attr: TokenStream, item: TokenStream) -> TokenStream {
    // stdout will captured by cargo.
    // simply use stderr to print msg.
    eprintln!("{:#?}", attr);
    eprintln!("{:#?}", item);

    item
}
