use std::marker::PhantomData;

use naga::{Expression, Handle, Type};

use crate::__private::ExpressionHandle;

use super::{
    block::BlockBuilder,
    error::BuilderError,
    function::{
        Captures, FunctionBuilder, GenerateFunction
    },
    r#type::TypeHandle,
};


pub struct ClosureFoo<Args, Result, Body> {
    _args: PhantomData<Args>,
    _result: PhantomData<Result>,
    _body: PhantomData<Body>,
}


pub trait FunctionTrait<Args, Return>: 'static {

}


pub(super) fn create_closure<
    Closure: FunctionTrait<Args, Return>,
    Args: 'static,
    Return: 'static,
    Body: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static,
    //Call: Fn(&mut BlockBuilder, ExpressionHandle<Closure>) + 'static,   
>(block_builder: &mut BlockBuilder, body: Body) -> Result<ExpressionHandle<Closure>, BuilderError> {
    let (_ty, captures) = block_builder
        .function_builder
        .module_builder
        .create_closure::<Closure, _>(&ClosureBodyGenerator { body })?;

    // compose expression for captures struct
    let expr = if let Some(captures) = captures {
        block_builder.function_builder.add_expression(Expression::Compose {
            ty: captures.ty,
            components: captures.values,
        })?
    }
    else {
        ExpressionHandle::empty()
    };

    Ok(expr)
}

pub struct ClosureBodyGenerator<B> {
    body: B,
}

impl<B: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static> GenerateFunction
    for ClosureBodyGenerator<B>
{
    fn generate(&self, function_builder: &mut FunctionBuilder) -> Result<(), BuilderError> {
        (self.body)(function_builder)?;
        Ok(())
    }
}

pub struct ClosureCallGenerator<C> {
    call: C,
    captures: Captures,
}
