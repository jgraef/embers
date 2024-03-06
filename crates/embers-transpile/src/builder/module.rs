use std::{
    any::TypeId,
    collections::HashMap,
};

use naga::{
    Arena,
    EntryPoint,
    Function,
    ResourceBinding,
    Type,
    UniqueArena,
};

use super::{
    error::BuilderError,
    function::{
        FunctionBuilder,
        GenerateFunction,
    },
    pointer::AddressSpace,
    r#struct::StructBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
        Width,
    },
    variable::{
        GlobalVariable,
        GlobalVariableHandle,
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
    global_variables: Arena<naga::GlobalVariable>,
    global_variable_by_type_id: HashMap<String, GlobalVariableHandle>,
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        Self {
            by_type_id: Default::default(),
            types: Default::default(),
            struct_fields: Default::default(),
            functions: Default::default(),
            entry_points: vec![],
            global_variables: Default::default(),
            global_variable_by_type_id: Default::default(),
        }
    }
}

impl ModuleBuilder {
    pub fn add_type<T: 'static + ?Sized>(
        &mut self,
        name: Option<String>,
        naga_type_inner: naga::TypeInner,
    ) -> TypeHandle {
        let handle = self
            .types
            .insert(
                naga::Type {
                    name,
                    inner: naga_type_inner,
                },
                naga::Span::default(),
            )
            .into();

        self.by_type_id.insert(TypeId::of::<T>(), handle);

        handle
    }

    pub fn add_empty_type<T: 'static>(&mut self) -> TypeHandle {
        let ty = TypeHandle::default();
        self.by_type_id.insert(TypeId::of::<T>(), ty);
        ty
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

    pub fn add_struct(&mut self, name: impl ToString) -> StructBuilder {
        StructBuilder::new(self, name)
    }

    pub fn add_dynamic_array<T: ShaderType + Width>(&mut self) -> Result<TypeHandle, BuilderError> {
        // todo: add traits for naga::valid::TypeFlags and add Sized as trait bound here

        let base = self.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(self.add_empty_type::<T>());
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
            return Ok(self.add_empty_type::<T>());
        }
        let n = u32::try_from(N).map_err(|_| BuilderError::Invalid)?;

        let base = self.get_type_by_id_or_add_it::<T>()?;
        let Some(base) = base.get_data()
        else {
            return Ok(self.add_empty_type::<T>());
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

    pub fn get_type_by_id_or_add_it<T: ShaderType + ?Sized>(
        &mut self,
    ) -> Result<TypeHandle, BuilderError> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            Ok(*handle)
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<G: GenerateFunction>(
        &mut self,
        gen: &G,
    ) -> Result<TypeHandle, BuilderError> {
        let type_id = TypeId::of::<G>();
        if let Some(handle) = self.by_type_id.get(&type_id) {
            Ok(*handle)
        }
        else {
            let mut function_builder = FunctionBuilder::new::<G>(self);
            gen.generate(&mut function_builder)?;
            let handle = function_builder.build()?;
            self.by_type_id.insert(type_id, handle);
            Ok(handle)
        }
    }

    pub fn add_entrypoint<G: GenerateFunction>(&mut self, gen: G) -> Result<(), BuilderError> {
        let mut function_builder = FunctionBuilder::new::<G>(self);
        gen.generate(&mut function_builder)?;
        let entry_point = function_builder.build_entrypoint();
        self.entry_points.push(entry_point);

        Ok(())
    }

    pub fn get_global_variable_or_add_it<T: ShaderType + ?Sized, A: AddressSpace>(
        &mut self,
        name: impl Into<String>,
        binding: Option<ResourceBinding>,
    ) -> Result<GlobalVariable<T, A>, BuilderError> {
        let name = name.into();
        if let Some(handle) = self.global_variable_by_type_id.get(&name) {
            Ok(GlobalVariable::new(*handle))
        }
        else {
            let ty = self.get_type_by_id_or_add_it::<T>()?;

            let handle = if let Some(ty) = ty.get_data() {
                let handle = self.global_variables.append(
                    naga::GlobalVariable {
                        name: Some(name.clone()),
                        space: A::to_naga(),
                        binding,
                        ty,
                        init: None,
                    },
                    Default::default(),
                );
                GlobalVariableHandle::Handle(handle)
            }
            else {
                GlobalVariableHandle::Empty
            };

            self.global_variable_by_type_id.insert(name, handle);

            Ok(GlobalVariable::new(handle))
        }
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
