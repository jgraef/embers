use std::{
    any::{type_name, TypeId},
    collections::HashMap,
    marker::PhantomData, thread::Builder,
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
    ShaderStage, Type, TypeInner,
};

use super::{
    block::BlockBuilder, error::BuilderError, expression::{
        AsExpression,
        ExpressionHandle,
    }, module::ModuleBuilder, pointer::{
        AddressSpace,
        Pointer,
    }, r#struct::{align_to, layout_struct_members, StructField}, r#type::{
        AlignTo, ShaderType, TypeHandle, Width
    }, variable::{
        LetMutBinding,
        ScopeId,
    }
};
use crate::utils::try_all;

pub trait GenerateFunction: 'static {
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError>;
}

pub trait GenerateCall: 'static {
    type Return;
    fn call(
        &self,
        block_builder: &mut BlockBuilder,
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
        C: Fn(&mut BlockBuilder, TypeHandle) -> Result<ExpressionHandle<R>, BuilderError> + 'static,
        R: 'static,
    > GenerateCall for CallGenerator<B, C, R>
{
    type Return = R;

    fn call(&self, block_builder: &mut BlockBuilder) -> Result<ExpressionHandle<R>, BuilderError> {
        let func_handle = block_builder
            .function_builder
            .module_builder
            .get_func_by_id_or_add_it(self)?;
        let ret_handle = (self.call)(block_builder, func_handle)?;
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

pub struct InlineCallGenerator<B, R> {
    body: B,
    _return_type: PhantomData<R>,
}

impl<B, R> InlineCallGenerator<B, R> {
    pub fn new(body: B) -> Self {
        Self {
            body,
            _return_type: PhantomData,
        }
    }
}

impl<
        B: Fn(&mut BlockBuilder) -> Result<ExpressionHandle<R>, BuilderError> + 'static,
        R: 'static,
    > GenerateCall for InlineCallGenerator<B, R>
{
    type Return = R;

    fn call(&self, block_builder: &mut BlockBuilder) -> Result<ExpressionHandle<R>, BuilderError> {
        let ret_handle = (self.body)(block_builder)?;
        Ok(ret_handle)
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

impl<T: ?Sized> PhantomReceiver<T> {
    pub fn unpack(self) -> ExpressionHandle<T> {
        self.handle
    }
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
        _block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        Ok(self.handle.clone())
    }
}

/*
impl<T: ?Sized> IntoExpression<T> for PhantomReceiver<T> {
    fn into_expression(self) -> ExpressionHandle<T> {
        self.handle
    }
}
 */

impl<T: ?Sized> Clone for PhantomReceiver<T> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle,
        }
    }
}

impl<T: ?Sized> Copy for PhantomReceiver<T> {}

pub struct PhantomReceiverPointer<T: ShaderType + ?Sized, A: AddressSpace> {
    handle: ExpressionHandle<Pointer<T, A>>,
}

impl<T: ShaderType + ?Sized, A: AddressSpace> PhantomReceiverPointer<T, A> {
    pub fn unpack(self) -> ExpressionHandle<Pointer<T, A>> {
        self.handle
    }
}

impl<T: ShaderType + ?Sized, A: AddressSpace> std::ops::Deref for PhantomReceiverPointer<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("bug: PhantomReceiverPointer is not meant to be dereferenced")
    }
}

impl<T: ShaderType + ?Sized, A: AddressSpace> From<ExpressionHandle<Pointer<T, A>>>
    for PhantomReceiverPointer<T, A>
{
    fn from(handle: ExpressionHandle<Pointer<T, A>>) -> Self {
        Self { handle }
    }
}

impl<T: ShaderType + ?Sized, A: AddressSpace> AsExpression<Pointer<T, A>>
    for PhantomReceiverPointer<T, A>
{
    fn as_expression(
        &self,
        _block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Pointer<T, A>>, BuilderError> {
        Ok(self.handle.clone())
    }
}

/*
impl<T: ShaderType + ?Sized, A: AddressSpace> IntoExpression<Pointer<T, A>>
    for PhantomReceiverPointer<T, A>
{
    fn into_expression(self) -> ExpressionHandle<Pointer<T, A>> {
        self.handle
    }
}
 */

impl<T: ShaderType + ?Sized, A: AddressSpace> Clone for PhantomReceiverPointer<T, A> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle,
        }
    }
}

pub struct TypeHelper<T>(PhantomData<T>);

impl<T> TypeHelper<T> {
    pub fn from_argument(_: &T) -> Self {
        Self(PhantomData)
    }

    pub fn from_receiver(_: &PhantomReceiver<T>) -> Self {
        Self(PhantomData)
    }

    pub fn from_receiver_pointer<A: AddressSpace>(_: &PhantomReceiverPointer<T, A>) -> Self
    where
        T: ShaderType,
    {
        Self(PhantomData)
    }
}

impl<T> Clone for TypeHelper<T> {
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}

impl<T> Copy for TypeHelper<T> {}

impl<T: ShaderType + ?Sized, A: AddressSpace> Copy for PhantomReceiverPointer<T, A> {}

pub struct FunctionBuilder<'m> {
    pub module_builder: &'m mut ModuleBuilder,
    receiver: Option<TypeHandle>,
    pub(super) name: Option<String>,
    inputs: Vec<FunctionArgument>,
    output: TypeHandle,
    expressions: Arena<Expression>,
    named_expressions: FastIndexMap<Handle<Expression>, String>,
    local_variables: Arena<LocalVariable>,
    const_expressions: HashMap<Handle<Expression>, bool>,
    pub(super) body: Option<Block>,
    captures: Option<CapturesBuilder>,
    scope: ScopeId,
}

impl<'m> FunctionBuilder<'m> {
    pub fn new(module_builder: &'m mut ModuleBuilder, can_capture: bool) -> Self {
        let scope = module_builder.scope_id();
        let captures = can_capture.then(|| CapturesBuilder::default());
        Self {
            module_builder,
            receiver: None,
            name: None,
            inputs: vec![],
            output: TypeHandle::default(),
            expressions: Default::default(),
            named_expressions: Default::default(),
            local_variables: Default::default(),
            const_expressions: HashMap::default(),
            body: None,
            captures,
            scope,
        }
    }

    pub fn add_name(&mut self, name: impl ToString) {
        self.name = Some(name.to_string());
    }

    pub fn add_input_receiver<This: ShaderType>(
        &mut self,
        _: ExpressionHandle<This>,
    ) -> Result<FnInputBinding<This>, BuilderError> {
        assert!(self.inputs.is_empty());

        let ty = self.module_builder.get_type_by_id_or_add_it::<This>()?;
        self.receiver = Some(ty);

        if let Some(naga_ty) = ty.get_data() {
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
        _: ExpressionHandle<T>,
        is_mut: bool,
        binding: Option<Binding>,
    ) -> Result<FnInputBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        if let Some(naga_ty) = ty.get_data() {
            let i = self.inputs.len();
            self.inputs.push(FunctionArgument {
                ty: naga_ty,
                name: Some(ident.to_string()),
                binding,
            });
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

    pub fn add_expression<T: ?Sized>(
        &mut self,
        expr: Expression,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let is_const = self.expression_is_const(&expr)?;
        let handle = self.expressions.append(expr, Default::default());
        self.const_expressions.insert(handle, is_const);
        let mut handle = ExpressionHandle::new(handle);
        if is_const {
            handle.promote_const();
        }
        Ok(handle)
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

    pub fn add_local_variable<T: ShaderType>(
        &mut self,
        name: impl ToString,
        init: Option<ExpressionHandle<T>>,
    ) -> Result<LetMutBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        let let_mut = if let Some(ty) = ty.get_data() {
            let init = init
                .map(|init| {
                    if !init.is_const() {
                        return Err(BuilderError::NotConst);
                    }
                    init.try_get_handle()
                })
                .transpose()?;
            let handle = self.local_variables.append(
                LocalVariable {
                    name: Some(name.to_string()),
                    ty,
                    init,
                },
                Default::default(),
            );

            LetMutBinding::new(handle, self.scope)
        }
        else {
            LetMutBinding::empty(self.scope)
        };

        Ok(let_mut)
    }

    pub fn block<'f>(&'f mut self) -> BlockBuilder<'m, 'f> {
        BlockBuilder::new(self)
    }

    pub fn build(mut self) -> Result<(Function, Option<Captures>), BuilderError> {
        // add argument for captures
        let mut captures = None;
        if let Some(captures_builder) = self.captures {
            if let Some(input) = captures_builder.input {
                let arg_index = self.inputs.len();
                            
                let (members, span) = layout_struct_members(captures_builder.fields);
        
                let ty = self
                    .module_builder
                    .types
                    .insert(
                        naga::Type {
                            name: self.name.clone(),
                            inner: TypeInner::Struct {
                                members,
                                span,
                            }
                        },
                        naga::Span::default(),
                    );

                self.inputs.push(FunctionArgument {
                    name: Some("captures".to_owned()),
                    ty,
                    binding: None,
                });

                // replace placeholder expression
                *self.expressions.get_mut(input) = Expression::FunctionArgument(arg_index as _);

                captures = Some(Captures {
                    ty,
                    values: captures_builder.field_values,
                });
            }
        }

        let result = self
            .output
            .get_data()
            .map(|ty| FunctionResult { ty, binding: None });

        let naga_func = Function {
            name: self.name,
            arguments: self.inputs,
            result,
            local_variables: self.local_variables,
            expressions: self.expressions,
            named_expressions: self.named_expressions,
            body: self.body.unwrap_or_default(),
        };

        Ok((naga_func, captures))
    }

    pub(crate) fn expression_is_const(
        &mut self,
        expression: &Expression,
    ) -> Result<bool, BuilderError> {
        fn expression_handle_is_const(
            handle: naga::Handle<Expression>,
            module_builder: &ModuleBuilder,
            expressions: &Arena<Expression>,
            cache: &mut HashMap<Handle<Expression>, bool>,
        ) -> Result<bool, BuilderError> {
            if let Some(is_const) = cache.get(&handle) {
                Ok(*is_const)
            }
            else {
                let expression = expressions
                    .try_get(handle)
                    .map_err(|_| BuilderError::BadHandle)?;
                let is_const = expression_is_const(expression, module_builder, expressions, cache)?;
                cache.insert(handle, is_const);
                Ok(is_const)
            }
        }

        fn expression_is_const(
            expression: &Expression,
            module_builder: &ModuleBuilder,
            expressions: &Arena<Expression>,
            cache: &mut HashMap<Handle<Expression>, bool>,
        ) -> Result<bool, BuilderError> {
            let is_const = match expression {
                Expression::Literal(_) => true,
                Expression::Constant(_) => {
                    // todo we need to check override
                    false
                }
                Expression::ZeroValue(ty) => module_builder.type_is_fixed_size((*ty).into())?,
                Expression::Compose { components, .. } => {
                    try_all(components.iter().map(|handle| {
                        expression_handle_is_const(*handle, module_builder, expressions, cache)
                    }))?
                }
                Expression::Access { base, index } => {
                    expression_handle_is_const(*base, module_builder, expressions, cache)?
                        && expression_handle_is_const(*index, module_builder, expressions, cache)?
                }
                Expression::AccessIndex { base, .. } => {
                    expression_handle_is_const(*base, module_builder, expressions, cache)?
                }
                Expression::Splat { value, .. } => {
                    expression_handle_is_const(*value, module_builder, expressions, cache)?
                }
                Expression::Swizzle { vector, .. } => {
                    expression_handle_is_const(*vector, module_builder, expressions, cache)?
                }
                Expression::Unary { expr, .. } => {
                    expression_handle_is_const(*expr, module_builder, expressions, cache)?
                }
                Expression::Binary { left, right, .. } => {
                    expression_handle_is_const(*left, module_builder, expressions, cache)?
                        && expression_handle_is_const(*right, module_builder, expressions, cache)?
                }
                Expression::Select {
                    condition,
                    accept,
                    reject,
                } => {
                    expression_handle_is_const(*condition, module_builder, expressions, cache)?
                        && expression_handle_is_const(*accept, module_builder, expressions, cache)?
                        && expression_handle_is_const(*reject, module_builder, expressions, cache)?
                }
                Expression::Relational { argument, .. } => {
                    expression_handle_is_const(*argument, module_builder, expressions, cache)?
                }
                Expression::Math {
                    arg,
                    arg1,
                    arg2,
                    arg3,
                    ..
                } => {
                    expression_handle_is_const(*arg, module_builder, expressions, cache)?
                        && arg1
                            .map(|a| {
                                expression_handle_is_const(a, module_builder, expressions, cache)
                            })
                            .transpose()?
                            .unwrap_or(true)
                        && arg2
                            .map(|a| {
                                expression_handle_is_const(a, module_builder, expressions, cache)
                            })
                            .transpose()?
                            .unwrap_or(true)
                        && arg3
                            .map(|a| {
                                expression_handle_is_const(a, module_builder, expressions, cache)
                            })
                            .transpose()?
                            .unwrap_or(true)
                }
                Expression::As { expr, .. } => {
                    expression_handle_is_const(*expr, module_builder, expressions, cache)?
                }
                _ => false,
            };
            Ok(is_const)
        }

        expression_is_const(
            expression,
            &self.module_builder,
            &self.expressions,
            &mut self.const_expressions,
        )
    }

    pub fn capture<T: ShaderType + Width + AlignTo>(&mut self, scope: ScopeId, expression: ExpressionHandle<T>) -> Result<ExpressionHandle<T>, BuilderError> {
        let expression = if scope == self.scope {
            // we don't actually capture it because the variable is in this function's scope.
            expression
        }
        else {
            let captures = self.captures.as_mut().ok_or_else(|| BuilderError::FunctionCantCapture { name: self.name.clone() })?;

            if let Some(handle) = expression.get_handle() {
                // map outer expression to inner expression
                let handle = if let Some(handle) = captures.expressions.get(&handle) {
                    *handle
                }
                else {
                    // the inner expression will be field access into a generated struct
                    let input = captures.input.get_or_insert_with(|| {
                        // placeholder
                        self.expressions.append(Expression::FunctionArgument(0), Default::default())
                    });

                    let field_index = captures.fields.len();
                    captures.fields.push(StructField {
                        name: None,
                        ty: self.module_builder.get_type_by_id_or_add_it::<T>()?.try_get_data()?,
                        alignment: <T as AlignTo>::ALIGN_TO,
                        width: <T as Width>::WIDTH,
                    });

                    captures.field_values.push(handle);

                    let out_handle = self.expressions.append(
                        Expression::AccessIndex { base: *input, index: field_index as _ },
                        Default::default()
                    );

                    captures.expressions.insert(handle, out_handle);
                    out_handle
                };

                ExpressionHandle::<T>::new(handle)
            }
            else {
                // empty type
                expression
            }
        };
        Ok(expression)
    }
}

#[derive(Debug, Default)]
struct CapturesBuilder {
    /// handle for FunctionArgument expression
    input: Option<Handle<Expression>>,

    /// struct fields for captures
    fields: Vec<StructField>,

    field_values: Vec<Handle<Expression>>,

    /// maps outer expressions to inner expressions (access into captures struct)
    expressions: HashMap<Handle<Expression>, Handle<Expression>>,
}

pub struct Captures {
    pub ty: Handle<Type>,
    pub values: Vec<Handle<Expression>>,
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
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let expr = if let Some(index) = self.index {
            block_builder
                .function_builder
                .add_expression(Expression::FunctionArgument(index as u32))?
        }
        else {
            ExpressionHandle::empty()
        };

        Ok(expr)
    }
}
