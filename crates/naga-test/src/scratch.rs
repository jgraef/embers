use embers_transpile::builder::r#type::{ScalarKind, Width};

#[allow(non_camel_case_types)]
pub struct bool {
    _nonconstruct: (),
}
impl ScalarKind for bool {
    const KIND: naga::ScalarKind = naga::ScalarKind::Bool;
}
impl Width for bool {
    const WIDTH: usize = 1;
}
impl embers_transpile::shader_std::default::Default for bool {
    fn default() -> impl ::embers_transpile::__private::GenerateCall<Return = bool> {
        ::embers_transpile::__private::CallGenerator::new(
            move |
                mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder|
            {
                _function_builder.add_name("default");
                _function_builder.add_output::<bool>()?;
                let _tmp_macro_0 = {
                    #[allow(unused_variables)]
                    let mut function_builder = &mut _function_builder;
                    let ty_handle = function_builder
                        .module_builder
                        .get_type_by_id_or_add_it::<Self>()?
                        .try_get_data()?;
                    function_builder
                        .add_expression::<
                            Self,
                        >(embers_transpile::__private::naga::Expression::ZeroValue(ty_handle))?
                };
                let _tmp_macro_0 = ::embers_transpile::__private::AsExpression::as_expression(
                    &_tmp_macro_0,
                    &mut _function_builder,
                )?;
                let _tmp_macro_0: ::embers_transpile::__private::ExpressionHandle<
                    bool,
                > = _tmp_macro_0;
                _function_builder.add_emit(&_tmp_macro_0)?;
                _function_builder
                    .add_statement(::embers_transpile::__private::naga::Statement::Return {
                        value: _tmp_macro_0.get_handle(),
                    });
                ::embers_transpile::__private::Ok::<
                    (),
                    ::embers_transpile::__private::BuilderError,
                >(())
            },
            move |
                mut _function_builder: &mut ::embers_transpile::__private::FunctionBuilder,
                _func_handle: ::embers_transpile::__private::TypeHandle|
            {
                {
                    let _args = [].into_iter().flatten();
                    _function_builder.add_call(_func_handle, _args)
                }
            },
        )
    }
}