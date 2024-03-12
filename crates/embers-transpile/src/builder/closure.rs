use std::{
    marker::PhantomData,
    process::Output,
};

use naga::{
    Expression,
    Handle,
    Type,
};

use super::{
    block::BlockBuilder,
    error::BuilderError,
    function::{
        Captures,
        FunctionBuilder,
        FunctionTrait,
        GenerateFunction,
    },
    r#type::TypeHandle,
};
use crate::{
    __private::{
        AsExpression,
        ExpressionHandle,
    },
    shader_std::marker::TupleOfExpressionHandles,
};

/*
struct Closure<Body, Args, Output> {
    /// the type depends on Body so that it's unique, even if Args and Output
    /// are the same as another closures'
    _body: PhantomData<Body>,
    _args: PhantomData<Args>,
    _output: PhantomData<Output>,
}

impl<Body, , Output> FunctionTrait<Args>
    for Closure<Body, Args, Output>
{
    type Output = Output;

    fn call(
        func: ExpressionHandle<Self>,
        args: Args,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Self::Output>, BuilderError> {
        todo!()
    }
}

pub(super) fn create_closure<
    Body: Fn(&mut FunctionBuilder) -> Result<(), BuilderError> + 'static,
    Args,
    Output,
>(block_builder: &mut BlockBuilder, body: Body) -> Result<ExpressionHandle<impl FunctionTrait<Args, Output = Output>>, BuilderError> {


    let (_ty, captures) = block_builder
        .function_builder
        .module_builder
        .create_closure::<Closure<Body, Args, Output>, _>(&ClosureBodyGenerator { body })?;

    // compose expression for captures struct
    let expr = if let Some(captures) = captures {
        block_builder.function_builder.add_expression::<Closure<Body, Args, Output>>(Expression::Compose {
            ty: captures.ty,
            components: captures.values,
        })?
    }
    else {
        ExpressionHandle::empty()
    };

    Ok(expr)
}
 */
