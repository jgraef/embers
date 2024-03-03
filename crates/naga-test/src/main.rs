mod scratch;

use std::marker::PhantomData;

use color_eyre::eyre::Error;
use embers_transpile::{
    ricsl,
    RicslType,
};
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
            ChangeTag::Insert => ("ricsl", AnsiColors::Green),
            ChangeTag::Equal => ("     ", AnsiColors::Default),
        };
        let line = format!("{prefix} {change}");
        print!("{}", line.color(color));
    }
}

pub struct TypeCapture<T>(PhantomData<T>);

impl<T> TypeCapture<T> {
    pub fn new(_x: &T) -> Self {
        Self(PhantomData)
    }
}

#[derive(RicslType)]
struct Foo {
    x: u32,
}

impl Foo {
    fn bar(&self, x: u32) -> u32 {
        x * self.x
    }
}

#[ricsl]
fn bar(a: i32, b: i32) -> i32 {
    a + b
}

#[ricsl(entrypoint)]
fn foo() {
    //let a = Foo { x: 42u32 };
    //let b: u32 = a.x + 1u32;
    bar(1, 2);
}

fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    color_eyre::install()?;
    tracing_subscriber::fmt::init();

    //struct Foo(u32);
    //println!("{:?}", Foo);

    let wgsl = r#"
    struct Foo {
        x: u32,
    }
    @compute
    @workgroup_size(64, 1, 1)
    fn foo() {
        let a = Foo(42u);
        let b = a.x + 1u;
        //let c = *b;
    }
    fn bar(a: i32, b: i32) -> i32 {
        return a + b;
    }
    "#;

    let wgsl = naga::front::wgsl::parse_str(wgsl)?;
    //println!("{wgsl:#?}");

    let ricsl = foo()?;
    //print_module("ricsl", &module.naga);

    diff_modules(wgsl, ricsl.naga);

    Ok(())
}
