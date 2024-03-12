#![feature(arbitrary_self_types, generic_const_exprs)]
mod scratch;

use std::{
    any::type_name,
    fmt::Debug,
    marker::PhantomData,
};

use color_eyre::eyre::Error;
use embers_transpile::{
    builder::{
        function::FunctionBuilder,
        r#type::ShaderType,
    },
    transpile,
};
use itertools::Itertools;
use naga::{
    back::wgsl::WriterFlags,
    valid::{
        ValidationFlags,
        Validator,
    },
    Module,
};
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

fn to_wgsl(module: &naga::Module) -> Result<String, Error> {
    let mut validator = Validator::new(Default::default(), Default::default());
    let info = validator.validate(module)?;
    let wgsl = naga::back::wgsl::write_string(module, &info, WriterFlags::EXPLICIT_TYPES)?;
    Ok(wgsl)
}

/*
#[transpile]
mod shader {
    use shader_std::ops::Add;

    //fn index_range(global_id: vec3<u32>, num_workgroups: vec3<u32>) -> vec2<u32>
    // {    // todo: use all 3 workgroup dimensions.
    //    let id = global_id.y * (num_workgroups.x * WORKGROUP_SIZE_X) +
    // global_id.x;    let start_index = i32(id * p_chunk_size());
    //    let end_index = start_index + i32(p_chunk_size());
    //    return vec2<i32>(start_index, end_index);
    //}

    pub trait Op<T> {
        fn apply(lhs: T, rhs: T) -> T;
    }

    pub enum AddOp {}
    impl<T: Add<Output = T>> Op<T> for AddOp {
        fn apply(lhs: T, rhs: T) -> T {
            lhs + rhs
        }
    }

    #[transpile(entrypoint)]
    pub fn map<T: ShaderType + Width, O: Op<T>>(
        #[transpile(global(group = 0, binding = 0, address_space(storage(read))))]
        parameters: [i32],
        #[transpile(global(group = 0, binding = 1, address_space(storage(read))))] operand: [T],
        #[transpile(global(group = 0, binding = 2, address_space(storage(write))))]
        mut result: [T],
        #[transpile(builtin(global_invocation_id))] global_id: vec3<u32>,
    ) {
        let mut id = global_id.x;
    }
}
*/
#[transpile]
mod test {
    fn foo(a: u32, b: u32) -> u32 {
        a
    }

    #[transpile(entrypoint)]
    pub fn test(#[transpile(builtin(global_invocation_id))] global_id: vec3<u32>) {
        //let x = 42u32 + global_id.x;
        //foo(1u32, 2u32);
    }
}

fn main() -> Result<(), Error> {
    dotenvy::dotenv().ok();
    color_eyre::install()?;
    tracing_subscriber::fmt::init();

    //struct Foo(u32);
    //println!("{:?}", Foo);

    let wgsl = r#"
    @group(0)
    @binding(0)
    var<storage, read> readable: array<i32>;
    
    fn bar(a: u32, b: u32) -> u32 {
        return 3;
    }

    @compute
    @workgroup_size(64, 1, 1)
    fn foo() {
        bar(1, 2);
    }
    "#;

    //let wgsl = naga::front::wgsl::parse_str(wgsl)?;
    //println!("{wgsl:#?}");

    //use embers_transpile::shader_std;
    //let transpiled = shader::map::<shader_std::types::primitive::i32,
    // shader::AddOp>()?;
    let transpiled = test::test()?;
    println!("{:#?}", transpiled.naga);
    println!("");
    println!("{}", to_wgsl(&transpiled.naga)?);

    //print_side_by_side(&wgsl, &transpiled.naga);
    //diff_modules(wgsl, ricsl.naga);

    Ok(())
}
