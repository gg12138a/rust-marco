use proc_marco_definition::hello_world_proc_marco;

#[hello_world_proc_marco(blog(::org::example))]
fn foo(a: i32) {
    println!("foo msg: {a}");
}

fn main() {
    foo(2);
    println!("Hello, world!");
}
