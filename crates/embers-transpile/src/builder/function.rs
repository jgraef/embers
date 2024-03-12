use std::{
    any::{
        type_name,
        TypeId,
    },
    collections::HashMap,
    marker::PhantomData,
    ops::Deref,
    thread::Builder,
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
    Type,
    TypeInner,
};

use super::{
    block::BlockBuilder,
    error::BuilderError,
    expression::{
        AsExpression,
        DynExpressionHandle,
        ExpressionHandle,
    },
    module::ModuleBuilder,
    pointer::{
        AddressSpace,
        Pointer,
    },
    r#struct::{
        align_to,
        layout_struct_members,
        StructField,
    },
    r#type::{
        AlignTo,
        ShaderType,
        TypeHandle,
        Width,
    },
    variable::{
        LetMutBinding,
        ScopeId,
    },
};
use crate::{
    shader_std::{
        marker::TupleOfExpressionHandles,
        types::Unit,
    },
    utils::{
        self,
        try_all,
    },
    Module,
};

// todo: add trait bound for Args: TupleOfExpressionHandles
pub trait FunctionTrait: Sized {
    type Args: TupleOfExpressionHandles;
    type Output;

    fn call(
        func: ExpressionHandle<Self>,
        args: Self::Args,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Self::Output>, BuilderError>;
}

impl<F: FunctionTrait> ExpressionHandle<F> {
    pub fn call(
        &self,
        args: F::Args,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<F::Output>, BuilderError> {
        F::call(*self, args, block_builder)
    }
}

pub trait GenerateFunction: 'static {
    fn generate_body(
        &self,
        block_builder: &mut BlockBuilder,
        args: Vec<DynFnInputBinding>,
    ) -> Result<DynExpressionHandle, BuilderError>;

    fn generate_function(&self, function_builder: &mut FunctionBuilder)
        -> Result<(), BuilderError>;
}

/*impl<T: GenerateFunction + ?Sized> GenerateFunction for Box<T> {
    fn generate_body(
        &self,
        block_builder: &mut BlockBuilder,
        args: Vec<DynFnInputBinding>,
    ) -> Result<DynExpressionHandle, BuilderError> {
        self.deref().generate_body(block_builder, args)
    }

    fn generate_function(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        self.deref().generate_function(function_builder)
    }
}

pub struct FunctionType<Args, Output, Generator, SelfKind = NoSelf> {
    _self: PhantomData<SelfKind>,
    _args: PhantomData<Args>,
    _output: PhantomData<Output>,
    _generator: PhantomData<Generator>,
}

impl<Args, Output, Generator> FunctionType<Args, Output, Generator> {
    pub fn new(_: &Args, _: &ExpressionHandle<Output>, _: &Generator) -> Self {
        Self {
            _self: PhantomData,
            _args: PhantomData,
            _output: PhantomData,
            _generator: PhantomData,
        }
    }
}*/

pub trait MaybeSelf: Default {
    type Inner;

    fn into_argument(self) -> Argument<Self::Inner>;
}

pub enum NoSelf {}

pub struct Argument<T: ?Sized> {
    _type: PhantomData<T>,
}

impl<T: ?Sized> Default for Argument<T> {
    fn default() -> Self {
        Self { _type: PhantomData }
    }
}

impl<T: ?Sized> Argument<T> {
    pub fn from_expression_handle(_: ExpressionHandle<T>) -> Self {
        Self::default()
    }

    pub fn as_empty_expression_handle(&self) -> ExpressionHandle<T> {
        ExpressionHandle::empty()
    }
}

impl<T: ?Sized> Clone for Argument<T> {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl<T: ?Sized> Copy for Argument<T> {}

impl<T> MaybeSelf for Argument<T> {
    type Inner = T;

    fn into_argument(self) -> Argument<T> {
        self
    }
}

pub struct SelfArgument<T: ?Sized> {
    arg: Argument<T>,
}

impl<T: ?Sized> Default for SelfArgument<T> {
    fn default() -> Self {
        Self::from_argument(Default::default())
    }
}

impl<T: ?Sized> SelfArgument<T> {
    pub fn from_argument(arg: Argument<T>) -> Self {
        Self { arg }
    }

    pub fn as_empty_expression_handle(&self) -> ExpressionHandle<T> {
        self.arg.as_empty_expression_handle()
    }
}

impl<T: ?Sized> Deref for SelfArgument<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("SelfArgument must not be dereferenced");
    }
}

impl<T: ?Sized> Clone for SelfArgument<T> {
    fn clone(&self) -> Self {
        Self {
            arg: self.arg.clone(),
        }
    }
}

impl<T: ?Sized> Copy for SelfArgument<T> {}

impl<T> MaybeSelf for SelfArgument<T> {
    type Inner = T;

    fn into_argument(self) -> Argument<T> {
        self.arg
    }
}

pub struct SelfPointerArgument<T: ?Sized, A> {
    arg: Argument<Pointer<T, A>>,
}

impl<T: ?Sized, A> Default for SelfPointerArgument<T, A> {
    fn default() -> Self {
        Self::from_argument(Default::default())
    }
}

impl<T: ?Sized, A> SelfPointerArgument<T, A> {
    pub fn from_argument(arg: Argument<Pointer<T, A>>) -> Self {
        Self { arg }
    }

    pub fn into_argument(self) -> Argument<Pointer<T, A>> {
        self.arg
    }

    pub fn as_empty_expression_handle(&self) -> ExpressionHandle<Pointer<T, A>> {
        self.arg.as_empty_expression_handle()
    }
}

impl<T: ?Sized, A> Deref for SelfPointerArgument<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        panic!("SelfPointerArgument must not be dereferenced");
    }
}

impl<T: ?Sized, A> Clone for SelfPointerArgument<T, A> {
    fn clone(&self) -> Self {
        Self {
            arg: self.arg.clone(),
        }
    }
}

impl<T: ?Sized, A> Copy for SelfPointerArgument<T, A> {}

impl<T, A> MaybeSelf for SelfPointerArgument<T, A> {
    type Inner = Pointer<T, A>;

    fn into_argument(self) -> Argument<Pointer<T, A>> {
        self.arg
    }
}

pub trait FnItem {
    type FunctionType: ?Sized;
    type Output: ?Sized;

    /// The job of this function is to register the function type and return an
    /// expression for it.
    fn get_function_expression(
        self,
        module_builder: &mut ModuleBuilder,
    ) -> Result<ExpressionHandle<Self::FunctionType>, BuilderError>;
}

pub trait FnItemEntryPoint: FnItem<Output = Unit> {
    fn register_entrypoint(self, module_builder: &mut ModuleBuilder) -> Result<(), BuilderError>;
}

pub struct CallbackGenerator<B, F> {
    pub body_generator: B,
    pub function_generator: F,
}

impl<
        B: Fn(
                &mut BlockBuilder,
                Vec<DynFnInputBinding>,
            ) -> Result<DynExpressionHandle, BuilderError>
            + 'static,
        F: Fn(&mut FunctionBuilder, &B) -> Result<(), BuilderError> + 'static,
    > CallbackGenerator<B, F>
{
    pub fn new(body_generator: B, function_generator: F) -> Self {
        Self {
            body_generator,
            function_generator,
        }
    }
}

impl<
        B: Fn(
                &mut BlockBuilder,
                Vec<DynFnInputBinding>,
            ) -> Result<DynExpressionHandle, BuilderError>
            + 'static,
        F: Fn(&mut FunctionBuilder, &B) -> Result<(), BuilderError> + 'static,
    > GenerateFunction for CallbackGenerator<B, F>
{
    fn generate_body(
        &self,
        block_builder: &mut BlockBuilder,
        args: Vec<DynFnInputBinding>,
    ) -> Result<DynExpressionHandle, BuilderError> {
        (self.body_generator)(block_builder, args)
    }

    fn generate_function(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<(), BuilderError> {
        (self.function_generator)(function_builder, &self.body_generator)
    }
}

pub struct GeneratorFnItem<T: ?Sized, G> {
    generator: G,
    _type: PhantomData<T>,
}

impl<T: ?Sized, G> GeneratorFnItem<T, G> {
    pub fn new(generator: G) -> Self {
        Self {
            generator,
            _type: PhantomData,
        }
    }

    pub fn new_with_type(_: &T, generator: G) -> Self {
        Self::new(generator)
    }
}

impl<T: FunctionTrait + ?Sized + 'static, G: GenerateFunction> FnItem for GeneratorFnItem<T, G> {
    type FunctionType = T;
    type Output = <T as FunctionTrait>::Output;

    fn get_function_expression(
        self,
        module_builder: &mut ModuleBuilder,
    ) -> Result<ExpressionHandle<Self::FunctionType>, BuilderError> {
        module_builder.add_function::<T>(Box::new(self.generator))?;
        Ok(ExpressionHandle::empty())
    }
}

impl<T: FunctionTrait<Output = Unit> + ?Sized + 'static, G: GenerateFunction> FnItemEntryPoint
    for GeneratorFnItem<T, G>
{
    fn register_entrypoint(self, module_builder: &mut ModuleBuilder) -> Result<(), BuilderError> {
        module_builder.add_entrypoint(self.generator)
    }
}

/*
pub struct Return<R: ?Sized> {
    pub generator: Box<dyn GenerateFunction>,
    pub return_type: PhantomData<R>,
    pub func_id: TypeId,
}

pub fn return_function_with_generator<
    Args: 'static,
    Output: 'static,
    Generator: GenerateFunction + 'static,
>(
    _args: Args,
    generator: Generator,
) -> Return<Output> {
    let func_id = TypeId::of::<FunctionType<Args, Output, Generator>>();
    Return {
        generator: Box::new(generator),
        return_type: PhantomData,
        func_id,
    }
}

pub fn return_function_with_closure<
    Args: 'static,
    Output: 'static,
    Body: Fn(&mut BlockBuilder, Vec<DynFnInputBinding>) -> Result<DynExpressionHandle, BuilderError>
        + 'static,
    Func: Fn(&mut FunctionBuilder, &Body) -> Result<(), BuilderError> + 'static,
>(
    args: Args,
    body: Body,
    func: Func,
) -> Return<Output> {
    struct G<Body, Func> {
        body: Body,
        func: Func,
    }

    impl<
            Body: Fn(
                    &mut BlockBuilder,
                    Vec<DynFnInputBinding>,
                ) -> Result<DynExpressionHandle, BuilderError>
                + 'static,
            Func: Fn(&mut FunctionBuilder, &Body) -> Result<(), BuilderError> + 'static,
        > GenerateFunction for G<Body, Func>
    {
        fn generate_body(
            &self,
            block_builder: &mut BlockBuilder,
            args: Vec<DynFnInputBinding>,
        ) -> Result<DynExpressionHandle, BuilderError> {
            (self.body)(block_builder, args)
        }

        fn generate_function(
            &self,
            function_builder: &mut FunctionBuilder,
        ) -> Result<(), BuilderError> {
            (self.func)(function_builder, &self.body)
        }
    }

    return_function_with_generator(args, G { body, func })
}
 */
mod func_impls {
    // generate impls AsExpression and FunctionTrait for functions Fn(Argument<_>,
    // ...) -> Return<_> this is in a separate mod so that we can easily expand
    // it
    embers_transpile_macros::impl_functions!(1);
}

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

    pub fn add_input<T: ShaderType>(
        &mut self,
        name: Option<String>,
        _: Argument<T>,
        is_mut: bool,
        binding: Option<Binding>,
    ) -> Result<FnInputBinding<T>, BuilderError> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        if let Some(naga_ty) = ty.get_data() {
            let i = self.inputs.len();
            self.inputs.push(FunctionArgument {
                ty: naga_ty,
                name,
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
        let mut handle = ExpressionHandle::from_naga(handle);
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
        let handle = expr.try_get_naga()?;
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
                    init.try_get_naga()
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

                let ty = self.module_builder.types.insert(
                    naga::Type {
                        name: self.name.clone(),
                        inner: TypeInner::Struct { members, span },
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

    pub fn capture<T: ShaderType + Width + AlignTo>(
        &mut self,
        scope: ScopeId,
        expression: ExpressionHandle<T>,
    ) -> Result<ExpressionHandle<T>, BuilderError> {
        let expression = if scope == self.scope {
            // we don't actually capture it because the variable is in this function's
            // scope.
            expression
        }
        else {
            let captures = self.captures.as_mut().ok_or_else(|| {
                BuilderError::FunctionCantCapture {
                    name: self.name.clone(),
                }
            })?;

            if let Some(handle) = expression.get_naga() {
                // map outer expression to inner expression
                let handle = if let Some(handle) = captures.expressions.get(&handle) {
                    *handle
                }
                else {
                    // the inner expression will be field access into a generated struct
                    let input = captures.input.get_or_insert_with(|| {
                        // placeholder
                        self.expressions
                            .append(Expression::FunctionArgument(0), Default::default())
                    });

                    let field_index = captures.fields.len();
                    captures.fields.push(StructField {
                        name: None,
                        ty: self
                            .module_builder
                            .get_type_by_id_or_add_it::<T>()?
                            .try_get_data()?,
                        alignment: <T as AlignTo>::ALIGN_TO,
                        width: <T as Width>::WIDTH,
                    });

                    captures.field_values.push(handle);

                    let out_handle = self.expressions.append(
                        Expression::AccessIndex {
                            base: *input,
                            index: field_index as _,
                        },
                        Default::default(),
                    );

                    captures.expressions.insert(handle, out_handle);
                    out_handle
                };

                ExpressionHandle::<T>::from_naga(handle)
            }
            else {
                // empty type
                expression
            }
        };
        Ok(expression)
    }

    pub fn scope_id(&self) -> ScopeId {
        self.scope
    }
}

#[derive(Debug, Default)]
struct CapturesBuilder {
    /// handle for FunctionArgument expression
    input: Option<Handle<Expression>>,

    /// struct fields for captures
    fields: Vec<StructField>,

    field_values: Vec<Handle<Expression>>,

    /// maps outer expressions to inner expressions (access into captures
    /// struct)
    /// todo: we could also make this map DynExpressionHandle
    expressions: HashMap<Handle<Expression>, Handle<Expression>>,
}

pub struct Captures {
    pub ty: Handle<Type>,
    pub values: Vec<Handle<Expression>>,
}

#[derive(Debug)]
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

    pub fn as_dyn(&self) -> DynFnInputBinding {
        DynFnInputBinding::Input { index: self.index }
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

/// A type-erased input binding that might also be an expression handle (for
/// inline calling)
#[derive(Clone, Copy, Debug)]
pub enum DynFnInputBinding {
    Input { index: Option<usize> },
    Expression { dyn_handle: DynExpressionHandle },
}
