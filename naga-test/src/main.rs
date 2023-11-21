use std::{any::{TypeId, type_name}, cell::OnceCell, marker::PhantomData};

use embers_ricsl::{
    ricsl,
    RicslType,
    builder::{ModuleBuilder, FunctionGenerator},
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

#[derive(RicslType)]
struct Foo {
    x: u32,
}


#[ricsl(entrypoint)]
fn foo(
    #[ricsl(builtin(global_invocation_id))]
    a: u32
) {
    1u32
}

#[ricsl]
fn bar() -> u32 {
    x
}



fn main() {
    /*let module = naga::front::wgsl::parse_str(r#"
    fn foo(x: u32) -> u32 { return x; }
    fn main() { foo(42); }
    "#).unwrap();
    println!("{module:#?}");*/

    let mut module_builder = ModuleBuilder::default();

    let bar = bar();
    bar.generate(&mut module_builder);
}
