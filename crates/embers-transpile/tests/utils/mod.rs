#![allow(dead_code)]

use embers_transpile::{
    builder::{
        module::ModuleBuilder,
        r#type::{
            AlignTo, Name, Width
        },
    },
    ShaderType,
};
use naga::{
    Module,
    TypeInner,
};

pub fn module_for_type<T: ShaderType>() -> Module {
    let mut builder = ModuleBuilder::default();
    T::add_to_module(&mut builder).unwrap();
    builder.build().naga
}

pub fn naga_type_for_type<T: ShaderType>(name: &str) -> naga::Type {
    let module = module_for_type::<T>();
    for (_, ty) in module.types.iter() {
        if ty.name.as_ref().map(|n| n == name).unwrap_or_default() {
            return ty.clone();
        }
    }
    panic!("no type found");
}

pub fn assert_struct_size<T: ShaderType + Name>(size: u32) {
    let ty = naga_type_for_type::<T>(T::NAME);
    match ty.inner {
        TypeInner::Struct { span, .. } => {
            assert_eq!(span, size);
        }
        _ => panic!("incorrect TypeInner"),
    }
}

pub fn assert_struct_alignment<T: ShaderType + Name>(field_offsets: &[u32]) {
    let ty = naga_type_for_type::<T>(T::NAME);
    match ty.inner {
        TypeInner::Struct { members, .. } => {
            for (i, field) in members.iter().enumerate() {
                if field.offset != field_offsets[i] {
                    panic!(
                        "expected offset {}, but found {} for field #{i}",
                        field_offsets[i], field.offset
                    );
                }
            }
        }
        _ => panic!("incorrect TypeInner"),
    }
}

pub fn assert_type_alignment<T: AlignTo>(alignment: u32) {
    assert_eq!(T::ALIGN_TO, alignment);
}

pub fn assert_type_width<T: Width>(width: u32) {
    assert_eq!(T::WIDTH, width);
}
