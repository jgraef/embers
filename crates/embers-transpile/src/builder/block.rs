use naga::{
    Block,
    Expression,
    Function,
    Handle,
    Statement,
};

use super::{
    error::BuilderError,
    expression::{
        DynExpressionHandle,
        ExpressionHandle,
    },
    function::FunctionBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
};
use crate::__private::DynFnInputBinding;

pub struct BlockBuilder<'m, 'f> {
    pub function_builder: &'f mut FunctionBuilder<'m>,
    statements: Vec<Statement>,
}

impl<'m, 'f> BlockBuilder<'m, 'f> {
    pub fn new(function_builder: &'f mut FunctionBuilder<'m>) -> Self {
        Self {
            function_builder,
            statements: vec![],
        }
    }

    pub fn add_statement(&mut self, statement: Statement) -> Result<(), BuilderError> {
        self.statements.push(statement);
        Ok(())
    }

    pub fn add_emit<T: ?Sized>(&mut self, expr: &ExpressionHandle<T>) -> Result<(), BuilderError> {
        if let Some(handle) = expr.get_naga() {
            let range = naga::Range::new_from_bounds(handle, handle);
            self.add_statement(Statement::Emit(range))?;
        }
        Ok(())
    }

    /*
    pub fn add_method_call<R: 'static>(
        &mut self,
        method: Return<R>,
        arguments: impl IntoIterator<Item = DynExpressionHandle>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let func = self
            .function_builder
            .module_builder
            .add_function(method.func_id, method.generator)?;
        self.add_call::<R>(func.try_get_code()?, arguments)
    }
    */

    pub fn add_call<F: 'static, R: 'static>(
        &mut self,
        arguments: impl IntoIterator<Item = DynExpressionHandle>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let naga_func = self
            .function_builder
            .module_builder
            .get_type::<F>()?
            .try_get_code()?;

        let ret_type = self.function_builder.module_builder.get_type::<R>()?;

        let ret = if ret_type.is_zero_sized() {
            ExpressionHandle::<R>::empty()
        }
        else {
            self.function_builder
                .add_expression::<R>(Expression::CallResult(naga_func))?
        };

        let arguments = arguments
            .into_iter()
            .filter_map(|dyn_handle| dyn_handle.get_naga())
            .collect();

        self.add_statement(Statement::Call {
            function: naga_func,
            arguments,
            result: ret.get_naga(),
        })?;

        Ok(ret)
    }

    pub fn add_call_inline<F: 'static, R: 'static>(
        &mut self,
        arguments: impl IntoIterator<Item = DynExpressionHandle>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let generator = self
            .function_builder
            .module_builder
            .get_function_generator::<F>()?;

        let args = arguments
            .into_iter()
            .map(|dyn_handle| DynFnInputBinding::Expression { dyn_handle })
            .collect();

        let mut inline_block = BlockBuilder::new(&mut self.function_builder);
        let output = generator.generate_body(&mut inline_block, args)?;
        let block = inline_block.finish();
        if !block.is_empty() {
            self.add_statement(Statement::Block(block))?;
        }

        Ok(ExpressionHandle::from_dyn(output))
    }

    fn finish_inner(self) -> (Block, &'f mut FunctionBuilder<'m>) {
        (Block::from_vec(self.statements), self.function_builder)
    }

    pub fn finish(self) -> Block {
        let (block, _) = self.finish_inner();
        block
    }

    pub fn finish_root(self) {
        let (block, function_builder) = self.finish_inner();
        function_builder.body = Some(block);
    }
}
