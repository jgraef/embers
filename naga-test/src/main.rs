mod scratch;

use embers_ricsl::{
    ricsl,
    RicslType,
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

#[ricsl]
fn bar() -> i32 {
    let _ = -1i32;

    //return 0u32;
}

#[ricsl(entrypoint)]
fn foo() {
    let x = bar();
}

fn main() {
    /*let module = naga::front::wgsl::parse_str(r#"
    @compute
    @workgroup_size(64, 1, 1)
    fn main() {
        var a: u32;
        a = 2 * a;
    }
    "#).unwrap();
    println!("{module:#?}");*/

    let module = foo();
    println!("{module:#?}");
}
