use naga::{
    Expression,
    Handle,
};

use crate::{
    RicslType,
    __private::{
        BuilderError,
        ExpressionHandle,
        FunctionBuilder,
    },
};

pub trait FunctionGenerator<R: RicslType>: 'static {
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError>;
}

impl<F: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static, R: RicslType>
    FunctionGenerator<R> for F
{
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError> {
        self(function_builder)
    }
}

pub enum PhantomReceiver<T> {
    MethodCall(ExpressionHandle<T>),
    FunctionCall(T),
}

impl<T> std::ops::Deref for PhantomReceiver<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::MethodCall(_) => {
                panic!("fixme: PhantomReceiver::MethodCall is not meant to be dereferenced")
            }
            Self::FunctionCall(c) => c,
        }
    }
}

impl<T> From<ExpressionHandle<T>> for PhantomReceiver<T> {
    fn from(expr: ExpressionHandle<T>) -> Self {
        Self::MethodCall(expr)
    }
}

pub trait Callable<A, B, R> {
    fn call(
        &self,
        first: A,
        tail: B,
        handles: Vec<Handle<Expression>>,
        function_builder: &mut FunctionBuilder,
    ) -> Result<R, BuilderError>;
}

pub trait Composable<A, B>: RicslType {
    fn compose(
        first: A,
        tail: B,
        function_generator: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Self>, BuilderError>;
}

impl<F: Fn(A, B) -> G + 'static, A, B, G: FunctionGenerator<R>, R: RicslType>
    Callable<A, B, ExpressionHandle<R>> for F
{
    fn call(
        &self,
        first: A,
        tail: B,
        handles: Vec<Handle<Expression>>,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let gen = self(first, tail);
        let result = function_builder.add_call(self, gen, handles)?;
        Ok(result)
    }
}

/*impl<T: Composable<A, B>, A, B> Callable<A, B, ExpressionHandle<T>> for T {
    fn call(&self, first: A, tail: B, handles: Vec<Handle<Expression>>, function_builder: &mut FunctionBuilder) -> Result<ExpressionHandle<T>, BuilderError> {
        todo!()
    }
}*/
