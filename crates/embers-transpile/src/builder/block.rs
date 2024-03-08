use naga::{
    Block,
    Expression,
    Handle,
    Statement,
};

use super::{
    error::BuilderError,
    expression::ExpressionHandle,
    function::FunctionBuilder,
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

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

    pub fn add_call<R: ShaderType>(
        &mut self,
        function: TypeHandle,
        args: impl IntoIterator<Item = Handle<Expression>>,
    ) -> Result<ExpressionHandle<R>, BuilderError> {
        let naga_fun = function.try_get_code()?;

        let ret_type = self
            .function_builder
            .module_builder
            .get_type_by_id_or_add_it::<R>()?;

        let ret = if ret_type.is_zero_sized() {
            ExpressionHandle::<R>::from_empty()
        }
        else {
            let expr = Expression::CallResult(naga_fun);
            self.function_builder.add_expression::<R>(expr)?
        };

        self.add_statement(Statement::Call {
            function: naga_fun,
            arguments: args.into_iter().collect(),
            result: ret.get_handle(),
        })?;

        Ok(ret)
    }

    pub fn add_emit<T>(&mut self, expr: &ExpressionHandle<T>) -> Result<(), BuilderError> {
        if let Some(handle) = expr.get_handle() {
            let range = naga::Range::new_from_bounds(handle, handle);
            self.add_statement(Statement::Emit(range))?;
        }
        Ok(())
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
