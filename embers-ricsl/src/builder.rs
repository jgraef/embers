use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    sync::Arc,
    ops::Deref,
};

use naga::{
    Binding,
    Block,
    EntryPoint,
    Expression,
    Function,
    FunctionArgument,
    FunctionResult,
    LocalVariable,
    ShaderStage,
    Span,
    Statement,
};

use crate::{
    arena::{
        Arena,
        Handle,
    },
    RicslType,
};

#[derive(Debug)]
enum TypeInner {
    Instrinsic {
        naga: naga::Handle<naga::Type>,
    },
    Unit,
    Struct {
        naga: naga::Handle<naga::Type>,
        fields: Arena<StructField>,
        field_names: HashMap<String, Handle<StructField>>,
    },
    Func {
        naga: naga::Handle<naga::Function>,
        this: Option<Handle<Type>>,
        inputs: Vec<FunctionArgument>,
        output: Handle<Type>,
    },
}

#[derive(Debug)]
pub struct Type {
    name: Option<String>,
    inner: TypeInner,
    type_id: TypeId,
}

#[derive(Debug)]
pub struct ModuleBuilder {
    types: Arena<Type>,
    by_type_id: HashMap<TypeId, Handle<Type>>,
    naga_types: naga::UniqueArena<naga::Type>,
    unit_type: Handle<Type>,
    naga_functions: naga::Arena<naga::Function>,
    entry_points: Vec<EntryPoint>,
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        let mut types = Arena::default();
        let unit_type = types.insert(Type {
            name: Some("()".into()),
            inner: TypeInner::Unit,
            type_id: TypeId::of::<()>(),
        });

        Self {
            types,
            by_type_id: Default::default(),
            naga_types: Default::default(),
            unit_type,
            naga_functions: Default::default(),
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
        name: impl ToString,
        naga_type_inner: naga::TypeInner,
    ) -> Handle<Type> {
        let name = name.to_string();

        let naga_type = self.naga_types.insert(
            naga::Type {
                name: Some(name.clone()),
                inner: naga_type_inner,
            },
            naga::Span::default(),
        );

        let type_id = TypeId::of::<T>();
        let ty = Type {
            name: Some(name),
            inner: TypeInner::Instrinsic { naga: naga_type },
            type_id,
        };

        self.types.insert(ty)
    }

    pub fn get_type_by_id_or_add_it<T: RicslType>(&mut self) -> Handle<Type> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            *handle
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<F: 'static, G: FunctionGenerator>(
        &mut self,
        f: &F,
        g: G,
    ) -> Handle<Type> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<F>()) {
            *handle
        }
        else {
            let mut function_builder = FunctionBuilder::new::<F>(self);
            g.generate(&mut function_builder);
            function_builder.build()
        }
    }

    pub fn add_entrypoint<G: FunctionGenerator + 'static>(&mut self, g: G) {
        struct Entrypoint<T: 'static>(PhantomData<T>);

        let mut function_builder = FunctionBuilder::new::<Entrypoint<G>>(self);
        g.generate(&mut function_builder);
        let entry_point = function_builder.build_entrypoint();
        self.entry_points.push(entry_point);
    }

    fn get_naga_type(&self, handle: Handle<Type>) -> Option<naga::Handle<naga::Type>> {
        let ty = self.types.get(handle).unwrap();
        match &ty.inner {
            TypeInner::Instrinsic { naga } => Some(*naga),
            TypeInner::Struct { naga, .. } => Some(*naga),
            _ => None,
        }
    }

    fn get_naga_func(&self, handle: Handle<Type>) -> Option<naga::Handle<naga::Function>> {
        let ty = self.types.get(handle).unwrap();
        match &ty.inner {
            TypeInner::Func { naga, .. } => Some(*naga),
            _ => None,
        }
    }

    pub fn build(self) -> Module {
        let naga = naga::Module {
            types: self.naga_types,
            special_types: Default::default(),
            constants: Default::default(),
            global_variables: Default::default(),
            const_expressions: Default::default(),
            functions: self.naga_functions,
            entry_points: self.entry_points,
        };
        Module { naga }
    }
}

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    type_id: TypeId,
    name: String,
    fields: Arena<StructField>,
    field_names: HashMap<String, Handle<StructField>>,
}

impl<'a> StructBuilder<'a> {
    pub fn new<T: 'static>(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            type_id: TypeId::of::<T>(),
            name: name.to_string(),
            fields: Default::default(),
            field_names: Default::default(),
        }
    }

    pub fn add_named_field<T: RicslType>(&mut self, name: impl ToString) {
        self.add_field::<T>(Some(name.to_string()));
    }

    pub fn add_unnamed_field<T: RicslType>(&mut self) {
        self.add_field::<T>(None);
    }

    pub fn add_field<T: RicslType>(&mut self, name: Option<String>) {
        let field_type = self.module_builder.get_type_by_id_or_add_it::<T>();
        let handle = self.fields.insert(StructField {
            name: name.clone(),
            ty: field_type,
        });
        if let Some(name) = name {
            self.field_names.insert(name, handle);
        }
    }

    pub fn build(self) -> Handle<Type> {
        let mut members = vec![];
        for (_, field) in self.fields.iter() {
            let Some(naga_type) = self.module_builder.get_naga_type(field.ty)
            else {
                continue;
            };
            members.push(naga::StructMember {
                name: field.name.clone(),
                ty: naga_type,
                binding: None,
                offset: 0, // todo
            });
        }

        let naga_type = self.module_builder.naga_types.insert(
            naga::Type {
                name: Some(self.name.clone()),
                inner: naga::TypeInner::Struct { members, span: 0 },
            },
            naga::Span::default(),
        );

        let ty = Type {
            name: Some(self.name),
            inner: TypeInner::Struct {
                fields: self.fields,
                field_names: self.field_names,
                naga: naga_type,
            },
            type_id: self.type_id,
        };

        self.module_builder.types.insert(ty)
    }
}

#[derive(Debug)]
struct StructField {
    name: Option<String>,
    ty: Handle<Type>,
}

pub struct FunctionBuilder<'a> {
    pub module_builder: &'a mut ModuleBuilder,
    this: Option<Handle<Type>>,
    name: Option<String>,
    type_id: TypeId,
    inputs: Vec<FunctionArgument>,
    output: Handle<Type>,
    expressions: naga::Arena<Expression>,
    named_expressions: HashMap<String, naga::Handle<Expression>>,
    statements: Vec<Statement>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new<F: 'static>(module_builder: &'a mut ModuleBuilder) -> Self {
        let output = module_builder.unit_type;
        Self {
            module_builder,
            this: None,
            name: None,
            type_id: TypeId::of::<F>(),
            inputs: vec![],
            output,
            expressions: Default::default(),
            named_expressions: Default::default(),
            statements: vec![],
        }
    }

    pub fn add_name(&mut self, name: impl ToString) {
        self.name = Some(name.to_string());
    }

    pub fn add_input_receiver(&mut self) {
        // we need to check what kind of binding this is (self, &self, &mut self), and
        // then decide what type/pointer to use
        assert_eq!(self.inputs.len(), 0);
        todo!();
    }

    pub fn add_input_named<T: RicslType>(
        &mut self,
        ident: impl ToString,
        is_mut: bool,
        binding: Option<Binding>,
    ) -> FnInputBinding<T> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();
        if let Some(naga_ty) = self.module_builder.get_naga_type(ty) {
            self.inputs.push(FunctionArgument {
                ty: naga_ty,
                name: Some(ident.to_string()),
                binding,
            });
            let i = self.inputs.len();
            FnInputBinding::new(i, is_mut)
        }
        else {
            FnInputBinding::phantom(is_mut)
        }
    }

    pub fn add_input_wild<T: RicslType>(&mut self) -> FnInputBinding<T> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();
        FnInputBinding::phantom(false)
    }

    pub fn add_output<T: RicslType>(&mut self) {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();
        self.output = ty;
    }

    pub fn add_expression<T>(&mut self, expr: Expression) -> ExpressionHandle<T> {
        let handle = self.expressions.append(expr, Default::default());
        ExpressionHandle::from_handle(handle)
    }

    pub fn name_expression(&mut self, name: impl ToString, expr: naga::Handle<Expression>) {
        self.named_expressions.insert(name.to_string(), expr);
    }

    pub fn add_call<F: 'static, G: FunctionGenerator<Return = R>, R: RicslType>(
        &mut self,
        f: &F,
        gen: G,
        args: Vec<naga::Handle<Expression>>,
    ) -> ExpressionHandle<R> {
        let type_handle = self.module_builder.get_func_by_id_or_add_it(f, gen);

        //let fun_type = self.module_builder.types.get(type_handle).unwrap();
        let naga_fun = self.module_builder.get_naga_func(type_handle).unwrap();

        let ret = if R::PHANTOM {
            ExpressionHandle::<R>::from_phantom()
        }
        else {
            let expr = Expression::CallResult(naga_fun);
            self.add_expression::<R>(expr)
        };

        self.add_statement(Statement::Call {
            function: naga_fun,
            arguments: args,
            result: ret.handle(),
        });

        ret
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    fn build_naga(&mut self) -> naga::Function {
        let result = self
            .module_builder
            .get_naga_type(self.output)
            .map(|ty| FunctionResult { ty, binding: None });

        let naga_func = Function {
            name: self.name.clone(),
            arguments: std::mem::replace(&mut self.inputs, vec![]),
            result,
            local_variables: Default::default(),
            expressions: std::mem::replace(&mut self.expressions, Default::default()),
            named_expressions: Default::default(),
            body: Block::from_vec(std::mem::replace(&mut self.statements, vec![])),
        };

        naga_func
    }

    pub fn build_entrypoint(mut self) -> EntryPoint {
        let function = self.build_naga();
        EntryPoint {
            name: self.name.unwrap(),
            stage: ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size: [64, 1, 1],
            function,
        }
    }

    pub fn build(mut self) -> Handle<Type> {
        let naga_func = self.build_naga();

        let naga = self
            .module_builder
            .naga_functions
            .append(naga_func, Default::default());

        let handle = self.module_builder.types.insert(Type {
            name: self.name,
            inner: TypeInner::Func {
                naga,
                this: self.this,
                inputs: self.inputs,
                output: self.output,
            },
            type_id: self.type_id,
        });

        handle
    }
}

/*
#[derive(Debug)]
enum FunctionArgument {
    Receiver,
    Named {
        ty: Handle<Type>,
        binding: Option<Binding>,
        ident: String,
        is_mut: bool,
    },
    Wild {
        ty: Handle<Type>,
    },
} */

pub trait FunctionGenerator {
    type Return;

    fn generate(&self, function_builder: &mut FunctionBuilder);
}

#[derive(Debug)]
pub struct Module {
    naga: naga::Module,
}

#[derive(Debug)]
#[must_use]
pub enum ExpressionHandle<T> {
    Handle {
        handle: naga::Handle<Expression>,
        _ty: PhantomData<T>,
    },
    Phantom {
        _ty: PhantomData<T>,
    },
    Const {
        value: Arc<T>,
    },
}

impl<T> ExpressionHandle<T> {
    pub fn from_handle(handle: naga::Handle<Expression>) -> Self {
        Self::Handle {
            handle,
            _ty: PhantomData,
        }
    }

    pub fn from_phantom() -> Self {
        Self::Phantom { _ty: PhantomData }
    }

    pub fn from_constant(value: T) -> Self {
        Self::Const {
            value: Arc::new(value),
        }
    }

    pub fn handle(&self) -> Option<naga::Handle<Expression>> {
        match self {
            ExpressionHandle::Handle { handle, _ty } => Some(*handle),
            _ => None,
        }
    }

    pub fn constant(&self) -> Option<&T> {
        match self {
            ExpressionHandle::Const { value } => Some(value),
            _ => None,
        }
    }
}

impl<T: 'static> ExpressionHandle<T> {
    pub fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl<T> Clone for ExpressionHandle<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Handle { handle, _ty } => {
                Self::Handle {
                    handle: *handle,
                    _ty: PhantomData,
                }
            }
            Self::Phantom { _ty } => Self::Phantom { _ty: PhantomData },
            Self::Const { value } => {
                Self::Const {
                    value: value.clone(),
                }
            }
        }
    }
}

pub trait IntoExpressionHandle<T> {
    type Error;

    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, Self::Error>;
}

pub struct FnInputBinding<T: Sized> {
    index: Option<usize>,
    is_mut: bool,
    _ty: PhantomData<T>,
}

impl<T> FnInputBinding<T> {
    pub fn new(index: usize, is_mut: bool) -> Self {
        Self {
            index: Some(index),
            is_mut,
            _ty: PhantomData,
        }
    }

    pub fn phantom(is_mut: bool) -> Self {
        Self {
            index: None,
            is_mut,
            _ty: PhantomData,
        }
    }
}

impl<T> IntoExpressionHandle<T> for FnInputBinding<T> {
    type Error = ();

    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, Self::Error> {
        let expr = if let Some(index) = self.index {
            function_builder.add_expression(Expression::FunctionArgument(index as u32))
        }
        else {
            ExpressionHandle::from_phantom()
        };

        Ok(expr)
    }
}

impl<T> Deref for FnInputBinding<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("FnInputBinding<T> is not meant to be dereferenced");
    }
}

pub struct LetBinding<T> {
    value: Option<ExpressionHandle<T>>,
}

impl<T> LetBinding<T> {
    pub fn unbound() -> Self {
        Self { value: None }
    }

    pub fn from_expr(expr: ExpressionHandle<T>) -> Self {
        Self { value: Some(expr) }
    }
}

impl<T> IntoExpressionHandle<T> for LetBinding<T> {
    type Error = LetUnboundError;

    fn into_expr(
        self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, Self::Error> {
        self.value.ok_or(LetUnboundError)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("let uninitialized")]
pub struct LetUnboundError;

pub struct Var<T> {
    handle: Option<naga::Handle<LocalVariable>>,
    _ty: PhantomData<T>,
}

impl<T> Var<T> {
    pub fn new(handle: naga::Handle<LocalVariable>) -> Self {
        Self {
            handle: Some(handle),
            _ty: PhantomData,
        }
    }
}

impl<T> IntoExpressionHandle<T> for Var<T> {
    type Error = ();

    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, Self::Error> {
        let expr = if let Some(handle) = self.handle {
            //let pointer_expr =
            // function_builder.add_expression(Expression::LocalVariable(self.handle));
            let pointer = function_builder
                .expressions
                .append(Expression::LocalVariable(handle), Default::default());
            function_builder.add_expression::<T>(Expression::Load { pointer })
        }
        else {
            ExpressionHandle::from_phantom()
        };

        Ok(expr)
    }
}

pub struct FuncConst<F, A, G> {
    pub f: F,
    _ty: PhantomData<(A, G)>,
}

impl<F: Fn(A) -> G, A, G> IntoExpressionHandle<FuncConst<F, A, G>> for F {
    type Error = ();

    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<FuncConst<F, A, G>>, Self::Error> {
        Ok(ExpressionHandle::from_constant(FuncConst {
            f: self,
            _ty: PhantomData,
        }))
    }
}
