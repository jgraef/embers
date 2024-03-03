pub use ::naga;
pub use ::std::{
    self,
    marker::PhantomData,
    option::Option::{
        self,
        None,
        Some,
    },
    result::Result::{
        self,
        Err,
        Ok,
    },
    string::String,
    vec,
    vec::Vec,
};

pub use crate::{
    builder::{
        error::BuilderError,
        expression::{
            AsExpression,
            ExpressionHandle,
        },
        function::{
            Callable,
            FunctionBuilder,
            FunctionGenerator,
            PhantomReceiver,
        },
        module::{
            Module,
            ModuleBuilder,
        },
        r#struct::{
            FieldAccess,
            FieldAccessor,
        },
        r#type::{
            ShaderType,
            TypeHandle,
        },
    },
    shader_std,
};
