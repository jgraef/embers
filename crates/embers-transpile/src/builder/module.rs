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
        Width,
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
    pub fn add_type<T: 'static + ?Sized>(
        &mut self,
        name: Option<String>,
        naga_type_inner: naga::TypeInner,
    ) -> TypeHandle {
        let handle = self.types.insert(
            naga::Type {
                name,
                inner: naga_type_inner,
            },
            naga::Span::default(),
        );

        self.by_type_id
            .insert(TypeId::of::<T>(), TypeHandle::Type(handle));

        handle.into()
    }

    pub fn add_empty_type<T: 'static>(&mut self) -> TypeHandle {
        self.by_type_id.insert(TypeId::of::<T>(), TypeHandle::Empty);
        TypeHandle::Empty
    }

    pub fn add_scalar<T: ShaderType + Width>(&mut self, kind: naga::ScalarKind) -> TypeHandle {
        self.add_type::<T>(
            None,
            naga::TypeInner::Scalar(naga::Scalar {
                kind,
                width: T::WIDTH as u8,
            }),
        )
    }

    pub fn add_struct<T: 'static>(&mut self, name: impl ToString) -> StructBuilder {
        StructBuilder::new(self, name)
    }

    pub fn add_dynamic_array<T: ShaderType + Width>(&mut self) -> Result<TypeHandle, BuilderError> {
        // todo: add traits for naga::valid::TypeFlags and add Sized as trait bound here

        let ty = self.get_type_by_id_or_add_it::<T>()?;
        let base = match ty {
            TypeHandle::Empty => return Ok(self.add_empty_type::<T>()),
            TypeHandle::Func(_) => return Err(BuilderError::NotANagaType { ty }),
            TypeHandle::Type(ty) => ty,
        };

        Ok(self.add_type::<[T]>(
            None,
            naga::TypeInner::Array {
                base,
                size: naga::ArraySize::Dynamic,
                stride: <T as Width>::WIDTH as u32,
            },
        ))
    }

    pub fn add_sized_array<T: ShaderType + Width, const N: usize>(
        &mut self,
    ) -> Result<TypeHandle, BuilderError> {
        if N == 0 {
            return Ok(TypeHandle::Empty);
        }
        let n = u32::try_from(N).map_err(|_| BuilderError::Invalid)?;

        let ty = self.get_type_by_id_or_add_it::<T>()?;
        let base = match ty {
            TypeHandle::Empty => return Ok(self.add_empty_type::<T>()),
            TypeHandle::Func(_) => return Err(BuilderError::NotANagaType { ty }),
            TypeHandle::Type(ty) => ty,
        };

        Ok(self.add_type::<[T; N]>(
            None,
            naga::TypeInner::Array {
                base,
                size: naga::ArraySize::Constant(n.try_into().unwrap()),
                stride: <T as Width>::WIDTH as u32,
            },
        ))
    }

    pub fn get_type_by_id_or_add_it<T: ShaderType>(&mut self) -> Result<TypeHandle, BuilderError> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            Ok(*handle)
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
