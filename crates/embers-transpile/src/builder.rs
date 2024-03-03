use std::{
    any::{
        type_name,
        TypeId,
    },
    collections::HashMap,
    marker::PhantomData,
    sync::Arc,
};

use naga::{
    Arena,
    Binding,
    Block,
    EntryPoint,
    Expression,
    FastIndexMap,
    Function,
    FunctionArgument,
    FunctionResult,
    Handle,
    LocalVariable,
    ShaderStage,
    Span,
    Statement,
    StructMember,
    Type,
    UniqueArena,
};

use crate::{
    callable::FunctionGenerator,
    shader_std::ptr::{
        AddressSpace,
        Pointer,
    },
    FieldAccess,
    FieldAccessor,
    Module,
    RicslType,
};

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    #[error("type {ty} doesn't have a naga type")]
    NoNagaType { ty: &'static str },
    #[error("type {ty:?} is not a function")]
    NotAFunction { ty: TypeHandle },
    #[error("type {ty:?} is not a naga type")]
    NotANagaType { ty: TypeHandle },
    #[error("let is unbound")]
    LetUnbound,
    #[error("invalid")]
    Invalid,
}

#[derive(Copy, Clone, Debug)]
pub enum TypeHandle {
    Unit,
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

    pub fn try_get_type(&self) -> Result<Handle<Type>, BuilderError> {
        self.get_type()
            .ok_or_else(|| BuilderError::NotANagaType { ty: *self })
    }

    pub fn get_func(&self) -> Option<Handle<Function>> {
        match self {
            Self::Func(f) => Some(*f),
            _ => None,
        }
    }

    pub fn try_get_func(&self) -> Result<Handle<Function>, BuilderError> {
        self.get_func()
            .ok_or_else(|| BuilderError::NotAFunction { ty: *self })
    }

    pub fn is_phantom(&self) -> bool {
        match self {
            Self::Unit | Self::Phantom => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }
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
    struct_fields: HashMap<TypeId, Vec<Option<u32>>>,
    functions: Arena<Function>,
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

    pub fn get_type_by_id_or_add_it<T: RicslType>(&mut self) -> TypeHandle {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            *handle
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<F: 'static, G: FunctionGenerator<R>, R: RicslType>(
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

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    type_id: TypeId,
    name: String,
    fields: Vec<StructField>,
    field_index: u32,
    field_map: Vec<Option<u32>>,
}

impl<'a> StructBuilder<'a> {
    pub fn new<T: 'static>(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            type_id: TypeId::of::<T>(),
            name: name.to_string(),
            fields: vec![],
            field_index: 0,
            field_map: vec![],
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
        if let Some(ty) = field_type.get_type() {
            self.fields.push(StructField {
                name: name.clone(),
                ty,
            });
            self.field_map.push(Some(self.field_index));
            self.field_index += 1;
        }
        else {
            self.field_map.push(None);
        }
    }

    pub fn build(self) -> TypeHandle {
        let members = self
            .fields
            .into_iter()
            .map(|field| {
                StructMember {
                    name: field.name.clone(),
                    ty: field.ty,
                    binding: None,
                    offset: 0, // todo
                }
            })
            .collect::<Vec<_>>();

        let handle = if members.is_empty() {
            TypeHandle::Phantom
        }
        else {
            self.module_builder
                .struct_fields
                .insert(self.type_id, self.field_map);

            let handle = self.module_builder.types.insert(
                naga::Type {
                    name: Some(self.name.clone()),
                    inner: naga::TypeInner::Struct {
                        members,
                        span: 0, // todo
                    },
                },
                Span::default(),
            );

            TypeHandle::Type(handle)
        };

        self.module_builder.by_type_id.insert(self.type_id, handle);

        handle
    }
}

pub struct FunctionBuilder<'a> {
    pub module_builder: &'a mut ModuleBuilder,
    receiver: Option<TypeHandle>,
    name: Option<String>,
    type_id: TypeId,
    inputs: Vec<FunctionArgument>,
    output: TypeHandle,
    expressions: Arena<Expression>,
    named_expressions: FastIndexMap<Handle<Expression>, String>,
    statements: Vec<Statement>,
    local_variables: Arena<LocalVariable>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new<F: 'static>(module_builder: &'a mut ModuleBuilder) -> Self {
        let output = TypeHandle::Unit;
        Self {
            module_builder,
            receiver: None,
            name: None,
            type_id: TypeId::of::<F>(),
            inputs: vec![],
            output,
            expressions: Default::default(),
            named_expressions: Default::default(),
            statements: vec![],
            local_variables: Default::default(),
        }
    }

    pub fn add_name(&mut self, name: impl ToString) {
        self.name = Some(name.to_string());
    }

    pub fn add_input_receiver<This: RicslType>(&mut self) -> FnInputBinding<This> {
        assert!(self.inputs.is_empty());

        let ty = self.module_builder.get_type_by_id_or_add_it::<This>();
        self.receiver = Some(ty);

        if let Some(naga_ty) = ty.get_type() {
            self.inputs.push(FunctionArgument {
                name: Some("self".to_owned()),
                ty: naga_ty,
                binding: None,
            });

            FnInputBinding::new(0, false)
        }
        else {
            FnInputBinding::phantom(false)
        }
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

    pub fn name_expression<T>(
        &mut self,
        name: impl ToString,
        expr: ExpressionHandle<T>,
    ) -> Result<(), BuilderError> {
        let handle = expr.try_get_handle()?;
        self.named_expressions.insert(handle, name.to_string());
        Ok(())
    }

    pub fn add_call<'f, F: 'static, G: FunctionGenerator<R>, R: RicslType>(
        &mut self,
        f: &'f F,
        gen: G,
        args: Vec<Handle<Expression>>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let type_handle = self.module_builder.get_func_by_id_or_add_it(f, gen)?;
        let naga_fun = type_handle.try_get_func()?;

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
            result: ret.get_handle(),
        });

        Ok(ret)
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn add_local_variable<T: RicslType>(
        &mut self,
        name: impl ToString,
        init: Option<ExpressionHandle<T>>,
    ) -> Result<LetMutBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();
        let let_mut = if ty.is_phantom() {
            LetMutBinding::from_phantom()
        }
        else {
            let ty = ty.try_get_type()?;
            let init = init.map(|handle| handle.try_get_handle()).transpose()?;
            let handle = self.local_variables.append(
                LocalVariable {
                    name: Some(name.to_string()),
                    ty,
                    init,
                },
                Default::default(),
            );

            LetMutBinding::from_handle(handle)
        };

        Ok(let_mut)
    }

    pub fn add_emit<T>(&mut self, expr: &ExpressionHandle<T>) -> Result<(), BuilderError> {
        let handle = expr.try_get_handle()?;
        let range = naga::Range::new_from_bounds(handle, handle);
        self.add_statement(Statement::Emit(range));
        Ok(())
    }

    fn build_naga(&mut self) -> naga::Function {
        match self.statements.last() {
            Some(Statement::Return { .. }) => {}
            _ => {
                if self.output.is_unit() {
                    // todo: when does naga emit this?
                    //self.statements.push(Statement::Return { value: None });
                }
                else {
                    // naga will nag us later that we didn't return a value.
                }
            }
        }

        let result = self
            .output
            .get_type()
            .map(|ty| FunctionResult { ty, binding: None });

        let naga_func = Function {
            name: self.name.clone(),
            arguments: std::mem::replace(&mut self.inputs, vec![]),
            result,
            local_variables: std::mem::replace(&mut self.local_variables, Default::default()),
            expressions: std::mem::replace(&mut self.expressions, Default::default()),
            named_expressions: std::mem::replace(&mut self.named_expressions, Default::default()),
            body: Block::from_vec(std::mem::replace(&mut self.statements, vec![])),
        };

        naga_func
    }

    pub fn build_entrypoint(mut self) -> EntryPoint {
        let function = self.build_naga();
        EntryPoint {
            name: self.name.expect("entrypoint has no name"),
            stage: ShaderStage::Compute,
            early_depth_test: None,
            workgroup_size: [64, 1, 1],
            function,
        }
    }

    pub fn build(mut self) -> Result<TypeHandle, BuilderError> {
        let naga_func = self.build_naga();

        let handle = self
            .module_builder
            .functions
            .append(naga_func, Default::default());
        let handle = TypeHandle::Func(handle);

        self.module_builder.by_type_id.insert(self.type_id, handle);

        Ok(handle)
    }
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
    pub fn from_handle(handle: Handle<Expression>) -> Self {
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

    pub fn get_handle(&self) -> Option<Handle<Expression>> {
        match self {
            ExpressionHandle::Handle { handle, _ty } => Some(*handle),
            ExpressionHandle::Const { .. } => todo!("coerce constant into an expression"),
            _ => None,
        }
    }

    pub fn try_get_handle(&self) -> Result<Handle<Expression>, BuilderError> {
        self.get_handle().ok_or_else(|| {
            BuilderError::NoNagaType {
                ty: type_name::<T>(),
            }
        })
    }

    pub fn get_constant(&self) -> Option<&T> {
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

impl<T: RicslType, const A: AddressSpace> ExpressionHandle<Pointer<T, A>> {
    pub fn load(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let expr = match self {
            ExpressionHandle::Handle { handle, .. } => {
                let expr = function_builder.add_expression(Expression::Load { pointer: *handle });
                function_builder.add_emit(&expr)?;
                expr
            }
            ExpressionHandle::Phantom { _ty } => ExpressionHandle::from_phantom(),
            ExpressionHandle::Const { value } => todo!(),
        };

        Ok(expr)
    }

    pub fn store(
        &self,
        value: &ExpressionHandle<T>,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        match self {
            ExpressionHandle::Handle { handle, _ty } => {
                let value = value
                    .get_handle()
                    .expect("expected value to have a naga handle");
                function_builder.add_statement(Statement::Store {
                    pointer: *handle,
                    value,
                });
            }
            ExpressionHandle::Phantom { _ty } => {
                // we store a phantom (e.g. ()) by doing nothing
            }
            ExpressionHandle::Const { value } => {
                todo!("fixme: assigning a constant");
            }
        }

        Ok(())
    }
}

impl<T> AsExpression<T> for ExpressionHandle<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.clone())
    }
}

pub trait AsExpression<T> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError>;
}

pub trait Assign<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError>;
}

pub trait Dereference {
    type Target;

    fn dereference(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Target, BuilderError>;
}

pub trait AsPointer {
    type Pointer;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError>;
}

pub trait HasAddressSpace {
    const ADDRESS_SPACE: AddressSpace;
}

/*
pub trait IntoExpressionHandle<T: RicslType> {
    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError>;

    fn as_pointer_expr<const A: AddressSpace>(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError>;
}
 */

impl<T: RicslType, const A: AddressSpace> Dereference for ExpressionHandle<Pointer<T, A>> {
    type Target = ExpressionHandle<T>;

    fn dereference(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Target, BuilderError> {
        self.load(function_builder)
    }
}

pub struct FnInputBinding<T> {
    /// Some if this binding has a concrete naga type, None if it's a phantom.
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

impl<T: RicslType> AsExpression<T> for FnInputBinding<T> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;
        Ok(expr)
    }
}

impl<T: RicslType> AsPointer for FnInputBinding<T> {
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let expr = if let Some(index) = self.index {
            function_builder.add_expression(Expression::FunctionArgument(index as u32))
        }
        else {
            ExpressionHandle::from_phantom()
        };

        Ok(expr)
    }
}

impl<T> HasAddressSpace for FnInputBinding<T> {
    const ADDRESS_SPACE: AddressSpace = AddressSpace::Function;
}

/// A non-mutable let binding, represented by a named expression in naga, and
/// optionally initialized with an expression.
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

impl<T> AsExpression<T> for LetBinding<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        self.value.clone().ok_or(BuilderError::LetUnbound)
    }
}

impl<T: RicslType> AsPointer for LetBinding<T> {
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        // todo: create an abstract pointer. we might need to capture the mut-ness of
        // pointers, so we can't actually construct a mut-pointer here
        todo!("LetBinding::into_pointer_expr")
    }
}

/// A mutable let binding, represented by a local variable in naga.
pub struct LetMutBinding<T> {
    handle: Option<naga::Handle<LocalVariable>>,
    _ty: PhantomData<T>,
}

impl<T> LetMutBinding<T> {
    pub fn from_handle(handle: naga::Handle<LocalVariable>) -> Self {
        Self {
            handle: Some(handle),
            _ty: PhantomData,
        }
    }

    pub fn from_phantom() -> Self {
        Self {
            handle: None,
            _ty: PhantomData,
        }
    }
}

impl<T: RicslType> AsExpression<T> for LetMutBinding<T> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;

        Ok(expr)
    }
}

impl<T: RicslType> AsPointer for LetMutBinding<T> {
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let expr = if let Some(handle) = self.handle {
            function_builder.add_expression(Expression::LocalVariable(handle))
        }
        else {
            ExpressionHandle::from_phantom()
        };

        Ok(expr)
    }
}

impl<T: RicslType> Assign<T> for LetMutBinding<T> {
    fn assign<E: AsExpression<T>>(
        &self,
        value: E,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        let value = value.as_expression(function_builder)?;
        let pointer = self.as_pointer(function_builder)?;
        pointer.store(&value, function_builder)?;
        Ok(())
    }
}

impl<T> HasAddressSpace for LetMutBinding<T> {
    const ADDRESS_SPACE: AddressSpace = AddressSpace::Function;
}

/*pub struct FuncConst<F, A, B, G> {
    pub f: F,
    _ty: PhantomData<(A, B, G)>,
}

impl<F: 'static, A: 'static, B: 'static, G: 'static> RicslType for FuncConst<F, A, B, G> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> TypeHandle {
        //let handle = module_builder.get_func_by_id_or_add_it(&self.f, g)
        //TypeHandle::Func(handle)
        todo!("FuncConst::add_to_module")
    }
}

impl<F: Fn(A, B) -> G + 'static, A: 'static, B: 'static, G: 'static>
    AsExpression<FuncConst<F, A, B, G>> for F
{
    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<FuncConst<F, A, B, G>>, BuilderError> {
        Ok(ExpressionHandle::from_constant(FuncConst {
            f: self,
            _ty: PhantomData,
        }))
    }

    fn as_pointer_expr<const Ad: AddressSpace>(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<FuncConst<F, A, B, G>, Ad>>, BuilderError> {
        todo!("FuncConst::as_pointer_expr")
    }
} */

pub struct Field<T, U> {
    base: ExpressionHandle<T>,
    index: usize,
    _field_ty: PhantomData<U>,
}

impl<T, U> Field<T, U> {
    pub fn new<const FIELD: FieldAccessor>(base: ExpressionHandle<T>) -> Self
    where
        T: FieldAccess<FIELD, Type = U>,
    {
        Self {
            base,
            index: T::INDEX,
            _field_ty: PhantomData,
        }
    }
}

impl<T: RicslType, U: RicslType> AsExpression<U> for Field<T, U> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<U>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;
        Ok(expr)
    }
}

impl<T: RicslType, U: RicslType> AsPointer for Field<T, U> {
    // fixme
    type Pointer = ExpressionHandle<Pointer<U, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let field_map = function_builder
            .module_builder
            .struct_fields
            .get(&self.base.type_id());
        let base = self.base.get_handle();

        let handle = match (field_map, base) {
            (Some(field_map), Some(base)) => {
                field_map
                    .get(self.index)
                    .expect("field index out of bounds")
                    .map(|index| {
                        function_builder.add_expression(Expression::AccessIndex { base, index })
                    })
            }
            _ => None,
        };

        let handle = handle.unwrap_or_else(|| ExpressionHandle::from_phantom());

        Ok(handle)
    }
}
