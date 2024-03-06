#![feature(arbitrary_self_types)]

mod scratch;

use std::{
    fmt::Debug,
    marker::PhantomData,
};

use color_eyre::eyre::Error;
use embers_transpile::{
    global,
    transpile,
    ShaderType,
};
use itertools::Itertools;
use naga::Module;
use owo_colors::{
    AnsiColors,
    OwoColorize,
};
use similar::{
    ChangeTag,
    TextDiff,
};

/*

# What do we want?

 - generate shader to create compute pipeline
 - generate bind group to pass to a kernel
  - tensor bindings
   - generate functions to access bindings (encode/decode?)
  - parameter binding
   - generate functions to access parameters
 - cache kernels
 - construct kernels from different parts
  - depending on types used, we need to add code to the kernel
  - kernel can be generic
   - e.g. map, fold
   - over types (how though? there are no traits)
   - over number of inputs/outputs?
  - (pass lambdas to kernels; as const)
  - support custom "number" types
  - allow short-cuts like bitwise operations on packed bools.

  - compose kernels into a compute graph
   - parameters: tensors used for weights etc. loaded with the cg
   - inputs: tensors, passed to cg
   - outputs: tensors, result of cg





*/

fn print_module(title: &str, module: &Module) {
    println!("# {title}\n\n{module:#?}\n");
}

fn diff_modules(mut wgsl: Module, ricsl: Module) {
    wgsl.const_expressions.clear();

    let old = format!("{:#?}", wgsl);
    let new = format!("{:#?}", ricsl);
    let diff = TextDiff::from_lines(&old, &new);

    for change in diff.iter_all_changes() {
        let (prefix, color) = match change.tag() {
            ChangeTag::Delete => ("wgsl ", AnsiColors::Red),
            ChangeTag::Insert => ("tran ", AnsiColors::Green),
            ChangeTag::Equal => ("     ", AnsiColors::Default),
        };
        let line = format!("{prefix} {change}");
        print!("{}", line.color(color));
    }
}

fn print_side_by_side(left: &impl Debug, right: &impl Debug) {
    fn to_lines(x: &impl Debug, w: usize) -> Vec<String> {
        let text = format!("{x:#?}");
        textwrap::wrap(&text, w)
            .into_iter()
            .map(|s| s.into_owned())
            .collect()
    }

    let (width, _) = termion::terminal_size().unwrap();
    let text_width = width as usize / 2 - 2;

    let left = to_lines(left, text_width);
    let right = to_lines(right, text_width);

    for item in left.iter().zip_longest(right.iter()) {
        let (left, right) = item.left_and_right();
        let left = left.map(|s| s.as_str()).unwrap_or_default();
        let right = right.map(|s| s.as_str()).unwrap_or_default();
        assert!(left.len() <= text_width);
        print!("{}", left);
        for _ in 0..text_width - left.len() {
            print!(" ");
        }
        print!(" | ");
        println!("{}", right);
    }
}

mod shader {
    pub use embers_transpile::shader_std::prelude::*;

    //global! {
    //    #[embers(group = 0, binding = 0, address_space(storage(write)))]
    //    static parameters: [i32];
    //}

    #[transpile]
    fn bar(x: i32, y: i32) -> i32 {
        x + y
    }

    #[transpile]
    struct Foo(i32);

    #[transpile]
    impl Foo {
        pub fn bar(&self, x: &i32) -> i32 {
            *x + (*self).0
        }
    }

    #[transpile(entrypoint)]
    pub fn foo() {
        //let mut a = 0;
        //let b = &a;
        //let c = *b;
        //let z = bar(1, 2);
        //let x = parameters[0];
        //return;

        //let foo = Foo { 0: 42 };
        //let x = foo.bar();
    }
}

fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    color_eyre::install()?;
    tracing_subscriber::fmt::init();

    //struct Foo(u32);
    //println!("{:?}", Foo);

    let wgsl = r#"
    //@group(0)
    //@binding(0)
    //var<storage, read> readable: array<i32>;
    //@group(0)
    //@binding(1)
    //var<storage, read> writable: array<i32>;

    struct Foo {
        x: u32,
    }

    fn bar(_self: ptr<private, Foo>) -> i32 {
        return (*_self).x;
    }

    @compute
    @workgroup_size(64, 1, 1)
    fn foo() {
        //let x = parameters[0];
        //writable[0] = 1;
        //var a = 0;
        //let b = &a;
        //let c = *b;
        let foo = Foo(42);
        let x = bar(&foo);
    }
    "#;

    let wgsl = naga::front::wgsl::parse_str(wgsl)?;
    //println!("{wgsl:#?}");

    let transpiled = shader::foo()?;
    //println!("{:#?}", transpiled.naga);

    print_side_by_side(&wgsl, &transpiled.naga);
    //diff_modules(wgsl, ricsl.naga);

    Ok(())
}
