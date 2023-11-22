use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    ops::Deref,
    sync::Arc,
};

use naga::{
    Arena,
    Binding,
    Block,
    EntryPoint,
    Expression,
    Function,
    FunctionArgument,
    FunctionResult,
    Handle,
    LocalVariable,
    ShaderStage,
    Statement,
    StructMember,
    Type,
    UniqueArena,
};

use crate::RicslType;

#[derive(Copy, Clone, Debug)]
pub enum TypeHandle {
    Phantom,
    Type(Handle<Type>),
    Func(Handle<Function>),
}

impl From<Handle<Type>> for TypeHandle {
    fn from(value: Handle<Type>) -> Self {
        Self::Type(value)
    }
}

impl From<Handle<Function>> for TypeHandle {
    fn from(value: Handle<Function>) -> Self {
        Self::Func(value)
    }
}

impl TypeHandle {
    pub fn get_type(&self) -> Option<Handle<Type>> {
        match self {
            Self::Type(h) => Some(*h),
            _ => None,
        }
    }

    pub fn get_func(&self) -> Option<Handle<Function>> {
        match self {
            Self::Func(f) => Some(*f),
            _ => None,
        }
    }

    pub fn is_phantom(&self) -> bool {
        matches!(self, Self::Phantom)
    }
}

#[derive(Debug, Default)]
pub struct StructLayout {
    fields: Arena<StructField>,
    field_names: HashMap<String, Handle<StructField>>,
}

#[derive(Debug)]
struct StructField {
    name: Option<String>,
    ty: Handle<Type>,
}

#[derive(Debug)]
pub struct ModuleBuilder {
    by_type_id: HashMap<TypeId, TypeHandle>,
    types: UniqueArena<Type>,
    structs: HashMap<Handle<Type>, StructLayout>,
    functions: Arena<Function>,
    entry_points: Vec<EntryPoint>,
}

impl Default for ModuleBuilder {
    fn default() -> Self {
        Self {
            by_type_id: Default::default(),
            types: Default::default(),
            structs: Default::default(),
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
        name: impl ToString,
        naga_type_inner: naga::TypeInner,
    ) -> TypeHandle {
        let name = name.to_string();

        let handle = self.types.insert(
            naga::Type {
                name: Some(name.clone()),
                inner: naga_type_inner,
            },
            naga::Span::default(),
        );

        self.by_type_id
            .insert(TypeId::of::<T>(), TypeHandle::Type(handle));

        handle.into()
    }

    pub fn get_type_by_id_or_add_it<T: RicslType>(&mut self) -> TypeHandle {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            *handle
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<F: 'static, G: FunctionGenerator>(
        &mut self,
        _f: &F,
        g: G,
    ) -> TypeHandle {
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

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    type_id: TypeId,
    name: String,
    layout: StructLayout,
}

impl<'a> StructBuilder<'a> {
    pub fn new<T: 'static>(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            type_id: TypeId::of::<T>(),
            name: name.to_string(),
            layout: Default::default(),
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
        let ty = field_type.get_type().unwrap();
        let handle = self.layout.fields.append(
            StructField {
                name: name.clone(),
                ty,
            },
            Default::default(),
        );
        if let Some(name) = name {
            self.layout.field_names.insert(name, handle);
        }
    }

    pub fn build(self) -> TypeHandle {
        let members = self
            .layout
            .fields
            .iter()
            .map(|(_, field)| {
                StructMember {
                    name: field.name.clone(),
                    ty: field.ty,
                    binding: None,
                    offset: 0, // todo
                }
            })
            .collect();

        let handle = self.module_builder.types.insert(
            naga::Type {
                name: Some(self.name.clone()),
                inner: naga::TypeInner::Struct { members, span: 0 },
            },
            naga::Span::default(),
        );
        let handle = TypeHandle::Type(handle);

        self.module_builder.by_type_id.insert(self.type_id, handle);

        handle
    }
}

pub struct FunctionBuilder<'a> {
    pub module_builder: &'a mut ModuleBuilder,
    this: Option<TypeHandle>,
    name: Option<String>,
    type_id: TypeId,
    inputs: Vec<FunctionArgument>,
    output: TypeHandle,
    expressions: Arena<Expression>,
    named_expressions: HashMap<String, Handle<Expression>>,
    statements: Vec<Statement>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new<F: 'static>(module_builder: &'a mut ModuleBuilder) -> Self {
        let output = TypeHandle::Phantom;
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
        if let Some(naga_ty) = ty.get_type() {
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

    pub fn name_expression(&mut self, name: impl ToString, expr: Handle<Expression>) {
        self.named_expressions.insert(name.to_string(), expr);
    }

    pub fn add_call<F: 'static, G: FunctionGenerator<Return = R>, R: RicslType>(
        &mut self,
        f: &F,
        gen: G,
        args: Vec<Handle<Expression>>,
    ) -> ExpressionHandle<R> {
        let type_handle = self.module_builder.get_func_by_id_or_add_it(f, gen);
        let naga_fun = type_handle.get_func().expect("expected function");

        let ret_type = self.module_builder.get_type_by_id_or_add_it::<R>();

        let ret = if ret_type.is_phantom() {
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
            .output
            .get_type()
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

    pub fn build(mut self) -> TypeHandle {
        let naga_func = self.build_naga();

        let handle = self
            .module_builder
            .functions
            .append(naga_func, Default::default());
        let handle = TypeHandle::Func(handle);

        self.module_builder.by_type_id.insert(self.type_id, handle);

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
            ExpressionHandle::Const { .. } => todo!("coerce constant into an expression"),
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

pub struct PhantomReceiver<T>(PhantomData<T>);

impl<T> Deref for PhantomReceiver<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("PhantomReceiver<T> is not meant to be dereferenced");
    }
}

impl<T> From<ExpressionHandle<T>> for PhantomReceiver<T> {
    fn from(_value: ExpressionHandle<T>) -> Self {
        Self(PhantomData)
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

pub struct FuncConst<F, A, B, G> {
    pub f: F,
    _ty: PhantomData<(A, B, G)>,
}

impl<F: Fn(A, B) -> G, A, B, G> IntoExpressionHandle<FuncConst<F, A, B, G>> for F {
    type Error = ();

    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<FuncConst<F, A, B, G>>, Self::Error> {
        Ok(ExpressionHandle::from_constant(FuncConst {
            f: self,
            _ty: PhantomData,
        }))
    }
}
