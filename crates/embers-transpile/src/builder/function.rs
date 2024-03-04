use std::{
    any::TypeId,
    marker::PhantomData,
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
    Statement,
};

use super::{
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    module::ModuleBuilder,
    pointer::{
        AddressSpace,
        AsPointer,
        HasAddressSpace,
        Pointer,
    },
    r#type::{
        ShaderType,
        TypeHandle,
    },
    variable::LetMutBinding,
};

pub trait GenerateFunction: 'static {
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError>;
}

pub trait GenerateCall: 'static {
    type Return;
    fn call(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Self::Return>, BuilderError>;
}

pub struct CallGenerator<B, C, R> {
    body: B,
    call: C,
    _return_type: PhantomData<R>,
}

impl<B, C, R> CallGenerator<B, C, R> {
    pub fn new(body: B, call: C) -> Self {
        Self {
            body,
            call,
            _return_type: PhantomData,
        }
    }
}

impl<
        B: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static,
        C: Fn(&mut FunctionBuilder, TypeHandle) -> Result<ExpressionHandle<R>, BuilderError> + 'static,
        R: 'static,
    > GenerateCall for CallGenerator<B, C, R>
{
    type Return = R;

    fn call(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let func_handle = function_builder
            .module_builder
            .get_func_by_id_or_add_it(self)?;
        let ret_handle = (self.call)(function_builder, func_handle)?;
        Ok(ret_handle)
    }
}

impl<B: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static, C: 'static, R: 'static>
    GenerateFunction for CallGenerator<B, C, R>
{
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError> {
        (self.body)(function_builder)?;
        Ok(())
    }
}

pub struct EntrypointGenerator<B> {
    body: B,
}

impl<B> EntrypointGenerator<B> {
    pub fn new(body: B) -> Self {
        Self { body }
    }
}

impl<B: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static> GenerateFunction
    for EntrypointGenerator<B>
{
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError> {
        (self.body)(function_builder)?;
        Ok(())
    }
}

pub struct PhantomReceiver<T: ?Sized> {
    handle: ExpressionHandle<T>,
}

impl<T: ?Sized> std::ops::Deref for PhantomReceiver<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("bug: PhantomReceiver is not meant to be dereferenced")
    }
}

impl<T: ?Sized> From<ExpressionHandle<T>> for PhantomReceiver<T> {
    fn from(handle: ExpressionHandle<T>) -> Self {
        Self { handle }
    }
}

impl<T: ?Sized> AsExpression<T> for PhantomReceiver<T> {
    fn as_expression(
        &self,
        _function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.handle.clone())
    }
}

pub struct PhantomReceiverPointer<T: ?Sized, const ADDRESS_SPACE: AddressSpace> {
    _ty: PhantomData<T>,
}

impl<T: ?Sized, const ADDRESS_SPACE: AddressSpace> std::ops::Deref
    for PhantomReceiverPointer<T, ADDRESS_SPACE>
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("bug: PhantomReceiverPointer is not meant to be dereferenced")
    }
}

impl<T: ShaderType + ?Sized, const ADDRESS_SPACE: AddressSpace>
    From<ExpressionHandle<Pointer<T, { ADDRESS_SPACE }>>>
    for PhantomReceiverPointer<T, { ADDRESS_SPACE }>
{
    fn from(_handle: ExpressionHandle<Pointer<T, { ADDRESS_SPACE }>>) -> Self {
        Self { _ty: PhantomData }
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
        Self {
            module_builder,
            receiver: None,
            name: None,
            type_id: TypeId::of::<F>(),
            inputs: vec![],
            output: TypeHandle::Empty,
            expressions: Default::default(),
            named_expressions: Default::default(),
            statements: vec![],
            local_variables: Default::default(),
        }
    }

    pub fn add_name(&mut self, name: impl ToString) {
        self.name = Some(name.to_string());
    }

    pub fn add_input_receiver<This: ShaderType>(
        &mut self,
    ) -> Result<FnInputBinding<This>, BuilderError> {
        assert!(self.inputs.is_empty());

        let ty = self.module_builder.get_type_by_id_or_add_it::<This>()?;
        self.receiver = Some(ty);

        if let Some(naga_ty) = ty.get_type() {
            self.inputs.push(FunctionArgument {
                name: Some("self".to_owned()),
                ty: naga_ty,
                binding: None,
            });

            Ok(FnInputBinding::new(0, false))
        }
        else {
            Ok(FnInputBinding::empty(false))
        }
    }

    pub fn add_input_named<T: ShaderType>(
        &mut self,
        ident: impl ToString,
        is_mut: bool,
        binding: Option<Binding>,
    ) -> Result<FnInputBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        if let Some(naga_ty) = ty.get_type() {
            self.inputs.push(FunctionArgument {
                ty: naga_ty,
                name: Some(ident.to_string()),
                binding,
            });
            let i = self.inputs.len();
            Ok(FnInputBinding::new(i, is_mut))
        }
        else {
            Ok(FnInputBinding::empty(is_mut))
        }
    }

    pub fn add_input_wild<T: ShaderType>(&mut self) -> FnInputBinding<T> {
        FnInputBinding::empty(false)
    }

    pub fn add_output<T: ShaderType>(&mut self) -> Result<(), BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        self.output = ty;
        Ok(())
    }

    pub fn add_expression<T: ?Sized>(&mut self, expr: Expression) -> ExpressionHandle<T> {
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

    pub fn add_call<R: ShaderType>(
        &mut self,
        function: TypeHandle,
        args: impl IntoIterator<Item = Handle<Expression>>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let naga_fun = function.try_get_func()?;

        let ret_type = self.module_builder.get_type_by_id_or_add_it::<R>()?;

        let ret = if ret_type.is_empty() {
            ExpressionHandle::<R>::from_empty()
        }
        else {
            let expr = Expression::CallResult(naga_fun);
            self.add_expression::<R>(expr)
        };

        self.add_statement(Statement::Call {
            function: naga_fun,
            arguments: args.into_iter().collect(),
            result: ret.get_handle(),
        });

        Ok(ret)
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn add_local_variable<T: ShaderType>(
        &mut self,
        name: impl ToString,
        init: Option<ExpressionHandle<T>>,
    ) -> Result<LetMutBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        let let_mut = if ty.is_empty() {
            LetMutBinding::from_empty()
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
        if let Some(handle) = expr.get_handle() {
            let range = naga::Range::new_from_bounds(handle, handle);
            self.add_statement(Statement::Emit(range));
        }
        Ok(())
    }

    fn build_naga(&mut self) -> naga::Function {
        match self.statements.last() {
            Some(Statement::Return { .. }) => {}
            _ => {
                if self.output.is_empty() {
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

    pub fn empty(is_mut: bool) -> Self {
        Self {
            index: None,
            is_mut,
            _ty: PhantomData,
        }
    }
}

impl<T: ShaderType> AsExpression<T> for FnInputBinding<T> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;
        Ok(expr)
    }
}

impl<T: ShaderType> AsPointer for FnInputBinding<T> {
    type Pointer = ExpressionHandle<Pointer<T, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let expr = if let Some(index) = self.index {
            function_builder.add_expression(Expression::FunctionArgument(index as u32))
        }
        else {
            ExpressionHandle::from_empty()
        };

        Ok(expr)
    }
}

impl<T> HasAddressSpace for FnInputBinding<T> {
    const ADDRESS_SPACE: AddressSpace = AddressSpace::Function;
}
