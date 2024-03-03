use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
};

use naga::{
    Arena,
    EntryPoint,
    Function,
    Type,
    UniqueArena,
};

use super::{
    error::BuilderError,
    function::{
        FunctionBuilder,
        FunctionGenerator,
    },
    r#struct::StructBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

#[derive(Debug)]
pub struct Module {
    pub naga: naga::Module,
}

#[derive(Debug)]
pub struct ModuleBuilder {
    pub(super) by_type_id: HashMap<TypeId, TypeHandle>,
    pub(super) types: UniqueArena<Type>,
    pub(super) struct_fields: HashMap<TypeId, Vec<Option<u32>>>,
    pub(super) functions: Arena<Function>,
    entry_points: Vec<EntryPoint>,
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        Self {
            by_type_id: Default::default(),
            types: Default::default(),
            struct_fields: Default::default(),
            functions: Default::default(),
            entry_points: vec![],
        }
    }
}

impl ModuleBuilder {
    pub fn add_struct<T: 'static>(&mut self, name: impl ToString) -> StructBuilder {
        StructBuilder::new::<T>(self, name)
    }

    pub(crate) fn add_intrinsic_type<T: 'static>(
        &mut self,
        name: Option<&str>,
        naga_type_inner: naga::TypeInner,
    ) -> TypeHandle {
        let handle = self.types.insert(
            naga::Type {
                name: name.map(|n| n.to_owned()),
                inner: naga_type_inner,
            },
            naga::Span::default(),
        );

        self.by_type_id
            .insert(TypeId::of::<T>(), TypeHandle::Type(handle));

        handle.into()
    }

    pub fn get_type_by_id_or_add_it<T: ShaderType>(&mut self) -> TypeHandle {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            *handle
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<F: 'static, G: FunctionGenerator<R>, R: ShaderType>(
        &mut self,
        _f: &F,
        g: G,
    ) -> Result<TypeHandle, BuilderError> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<F>()) {
            Ok(*handle)
        }
        else {
            let mut function_builder = FunctionBuilder::new::<F>(self);
            g.generate(&mut function_builder)?;
            Ok(function_builder.build()?)
        }
    }

    pub fn add_entrypoint<G: FunctionGenerator<()>>(&mut self, gen: G) -> Result<(), BuilderError> {
        struct Entrypoint<T: 'static>(PhantomData<T>);

        let mut function_builder = FunctionBuilder::new::<Entrypoint<G>>(self);
        gen.generate(&mut function_builder)?;
        let entry_point = function_builder.build_entrypoint();
        self.entry_points.push(entry_point);

        Ok(())
    }

    pub fn build(self) -> Module {
        let naga = naga::Module {
            types: self.types,
            special_types: Default::default(),
            constants: Default::default(),
            global_variables: Default::default(),
            const_expressions: Default::default(),
            functions: self.functions,
            entry_points: self.entry_points,
        };
        Module { naga }
    }
}
