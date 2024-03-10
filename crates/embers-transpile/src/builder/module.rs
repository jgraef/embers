use std::{
    any::{
        type_name,
        TypeId,
    },
    collections::HashMap,
};

use naga::{
    Arena,
    EntryPoint,
    Function,
    ResourceBinding,
    ShaderStage,
    Type,
    UniqueArena,
};

use super::{
    error::BuilderError,
    function::{
        Captures,
        FunctionBuilder,
        GenerateFunction,
    },
    pointer::AddressSpace,
    r#struct::StructBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
    variable::{
        GlobalVariable,
        GlobalVariableHandle,
        ScopeId,
    },
};

#[derive(Debug)]
pub struct Module {
    pub naga: naga::Module,
}

pub struct ModuleBuilder {
    pub(super) by_type_id: HashMap<TypeId, TypeHandle>,
    pub(super) types: UniqueArena<Type>,
    pub(super) struct_fields: HashMap<TypeId, Vec<Option<u32>>>,
    pub(super) functions: Arena<Function>,
    function_generators: HashMap<TypeId, Box<dyn GenerateFunction>>,
    entry_points: Vec<EntryPoint>,
    global_variables: Arena<naga::GlobalVariable>,
    global_variable_by_type_id: HashMap<String, GlobalVariableHandle>,
    next_scope_id: usize,
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        Self {
            by_type_id: Default::default(),
            types: Default::default(),
            struct_fields: Default::default(),
            functions: Default::default(),
            function_generators: Default::default(),
            entry_points: vec![],
            global_variables: Default::default(),
            global_variable_by_type_id: Default::default(),
            next_scope_id: 0,
        }
    }
}

impl ModuleBuilder {
    pub fn add_naga_type<T: 'static + ?Sized>(
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

    pub fn add_struct(&mut self, name: impl ToString) -> StructBuilder {
        StructBuilder::new(self, name)
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

    pub fn add_function<G: GenerateFunction>(
        &mut self,
        func_id: TypeId,
        generator: G,
    ) -> Result<TypeHandle, BuilderError> {
        if let Some(handle) = self.by_type_id.get(&func_id) {
            Ok(*handle)
        }
        else {
            let mut function_builder = FunctionBuilder::new(self, false);
            generator.generate_function(&mut function_builder)?;

            let (function, captures) = function_builder.build()?;
            assert!(captures.is_none());
            let handle = self.functions.append(function, Default::default());
            let handle = handle.into();

            self.by_type_id.insert(func_id, handle);
            self.function_generators
                .insert(func_id, Box::new(generator));

            Ok(handle)
        }
    }

    pub fn get_type<T: 'static>(&mut self) -> Result<TypeHandle, BuilderError> {
        let type_id = TypeId::of::<T>();
        self.by_type_id.get(&type_id).copied().ok_or_else(|| {
            BuilderError::TypeNotFound {
                name: type_name::<T>().to_owned(),
            }
        })
    }

    pub fn add_entrypoint<G: GenerateFunction>(&mut self, gen: G) -> Result<(), BuilderError> {
        let mut function_builder = FunctionBuilder::new(self, false);
        gen.generate_function(&mut function_builder)?;

        let name = function_builder.name.clone();
        let (function, captures) = function_builder.build()?;
        assert!(captures.is_none());

        let entry_point = EntryPoint {
            name: name.expect("entrypoint has no name"),
            stage: ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size: [64, 1, 1],
            function,
        };
        self.entry_points.push(entry_point);

        Ok(())
    }

    pub fn get_global_variable_or_add_it<T: ShaderType + ?Sized, A: AddressSpace>(
        &mut self,
        name: String,
        binding: Option<ResourceBinding>,
    ) -> Result<GlobalVariable<T, A>, BuilderError> {
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

    pub fn type_is_fixed_size(&self, ty: TypeHandle) -> Result<bool, BuilderError> {
        if let Some(handle) = ty.data {
            let ty = self
                .types
                .get_handle(handle)
                .map_err(|_| BuilderError::BadHandle)?;
            Ok(!ty.inner.is_dynamically_sized(&self.types))
        }
        else {
            Ok(true)
        }
    }

    pub fn create_closure<C: 'static, G: GenerateFunction>(
        &mut self,
        generator: G,
    ) -> Result<(TypeHandle, Option<Captures>), BuilderError> {
        /*let type_id = TypeId::of::<C>();
        assert!(
            !self.by_type_id.contains_key(&type_id),
            "closure type was already registered"
        );

        let mut function_builder = FunctionBuilder::new(self, true);
        gen.generate_function(&mut function_builder)?;

        let (function, captures) = function_builder.build()?;
        let func_handle = self.functions.append(function, Default::default());
        let handle = TypeHandle {
            data: captures.as_ref().map(|captures| captures.ty),
            code: Some(func_handle),
        };

        self.by_type_id.insert(type_id, handle);
        Ok((handle, captures))*/

        // this can be merged with add_function
        todo!()
    }

    pub fn scope_id(&mut self) -> ScopeId {
        let id = self.next_scope_id;
        self.next_scope_id += 1;
        ScopeId(id)
    }
}
