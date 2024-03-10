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

    pub fn add_emit<T: ?Sized>(&mut self, expr: &ExpressionHandle<T>) -> Result<(), BuilderError> {
        if let Some(handle) = expr.get_naga() {
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
