pub mod error;
pub mod expression;
pub mod function;
pub mod module;
pub mod pointer;
pub mod r#struct;
pub mod r#type;
pub mod variable;

/*pub struct FuncConst<F, A, B, G> {
    pub f: F,
    _ty: PhantomData<(A, B, G)>,
}

impl<F: 'static, A: 'static, B: 'static, G: 'static> RicslType for FuncConst<F, A, B, G> {
    fn add_to_module(module_builder: &mut ModuleBuilder) -> TypeHandle {
        //let handle = module_builder.get_func_by_id_or_add_it(&self.f, g)
        //TypeHandle::Func(handle)
        todo!("FuncConst::add_to_module")
    }
}

impl<F: Fn(A, B) -> G + 'static, A: 'static, B: 'static, G: 'static>
    AsExpression<FuncConst<F, A, B, G>> for F
{
    fn into_expr(
        self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<FuncConst<F, A, B, G>>, BuilderError> {
        Ok(ExpressionHandle::from_constant(FuncConst {
            f: self,
            _ty: PhantomData,
        }))
    }

    fn as_pointer_expr<const Ad: AddressSpace>(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Pointer<FuncConst<F, A, B, G>, Ad>>, BuilderError> {
        todo!("FuncConst::as_pointer_expr")
    }
} */
