use std::marker::PhantomData;

use embers_transpile::{
    builder::{
        block::BlockBuilder,
        error::BuilderError,
        expression::{
            AsExpression,
            DynExpressionHandle,
            ExpressionHandle,
        },
        function::{
            Argument,
            CallbackGenerator,
            DynFnInputBinding,
            FunctionBuilder,
            FunctionTrait,
            MaybeTakesSelf,
            NoSelf,
        },
        module::ModuleBuilder,
    },
    shader_std::types::Unit,
};

pub fn transpiled_func(
    _self: NoSelf,
    _args: (Argument<u32>, Argument<u32>),
    module_builder: &mut ModuleBuilder,
) -> Result<
    ExpressionHandle<impl FunctionTrait<Args = (u32, u32)> + MaybeTakesSelf<SelfKind = NoSelf>>,
    BuilderError,
> {
    struct AnonymousType;

    impl FunctionTrait for AnonymousType {
        type Output = Unit;
        type Args = (u32, u32);

        fn call(
            func: ExpressionHandle<Self>,
            args: (ExpressionHandle<u32>, ExpressionHandle<u32>),
            block_builder: &mut BlockBuilder,
        ) -> Result<ExpressionHandle<Self::Output>, BuilderError> {
            todo!()
        }
    }

    impl MaybeTakesSelf for AnonymousType {
        type SelfKind = NoSelf;
    }

    let generator = CallbackGenerator::new(
        move |block_builder: &mut BlockBuilder, args: Vec<DynFnInputBinding>| {
            todo!();
        },
        move |function_builder: &mut FunctionBuilder, body_generator| {
            todo!();
        },
    );
    module_builder.add_function::<AnonymousType>(Box::new(generator));
    Ok(ExpressionHandle::<AnonymousType>::empty())
}

fn test_it() {
    let mut module_builder = ModuleBuilder::default();
    let mut function_builder = FunctionBuilder::new(&mut module_builder, false);
    let mut block_builder = function_builder.block();
    let x = AsExpression::as_expression(&transpiled_func, &mut block_builder).unwrap();
}
