use std::{
    any::TypeId,
    marker::{
        ConstParamTy,
        PhantomData,
    },
};

use naga::{
    Expression,
    Handle,
    StructMember,
    Type,
};

use super::{
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    function::FunctionBuilder,
    module::ModuleBuilder,
    pointer::{
        address_space,
        AddressSpace,
        AsPointer,
        DeferredDereference,
        Pointer,
    },
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

pub trait FieldAccessor {}

pub struct UnnamedFieldAccessor<const INDEX: usize>;
impl<const INDEX: usize> FieldAccessor for UnnamedFieldAccessor<{ INDEX }> {}

pub struct NamedFieldAccessor<const NAME: &'static str>;
impl<const NAME: &'static str> FieldAccessor for NamedFieldAccessor<{ NAME }> {}

pub trait FieldAccess<F: FieldAccessor> {
    type Type;
    type Result: AsExpression<Self::Type>;

    fn access(
        function_builder: &mut FunctionBuilder,
        base: ExpressionHandle<Self>,
    ) -> Result<Self::Result, BuilderError>;
}

#[derive(Debug)]
struct StructField {
    name: Option<String>,
    ty: Handle<Type>,
}

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    name: String,
    fields: Vec<StructField>,
    field_index: u32,
    field_map: Vec<Option<u32>>,
}

impl<'a> StructBuilder<'a> {
    pub fn new(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            name: name.to_string(),
            fields: vec![],
            field_index: 0,
            field_map: vec![],
        }
    }

    pub fn add_named_field<T: ShaderType>(
        &mut self,
        name: impl ToString,
    ) -> Result<(), BuilderError> {
        self.add_field::<T>(Some(name.to_string()))?;
        Ok(())
    }

    pub fn add_unnamed_field<T: ShaderType>(&mut self) -> Result<(), BuilderError> {
        self.add_field::<T>(None)?;
        Ok(())
    }

    pub fn add_field<T: ShaderType>(&mut self, name: Option<String>) -> Result<(), BuilderError> {
        let field_type = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        if let Some(ty) = field_type.get_data() {
            self.fields.push(StructField {
                name: name.clone(),
                ty,
            });
            self.field_map.push(Some(self.field_index));
            self.field_index += 1;
        }
        else {
            self.field_map.push(None);
        }
        Ok(())
    }

    pub fn build<T: ShaderType>(self) -> TypeHandle {
        let members = self
            .fields
            .into_iter()
            .map(|field| {
                StructMember {
                    name: field.name.clone(),
                    ty: field.ty,
                    binding: None,
                    offset: 0, // todo
                }
            })
            .collect::<Vec<_>>();

        let handle = if members.is_empty() {
            self.module_builder.add_empty_type::<T>()
        }
        else {
            self.module_builder
                .struct_fields
                .insert(TypeId::of::<T>(), self.field_map);

            self.module_builder.add_naga_type::<T>(
                Some(self.name),
                naga::TypeInner::Struct {
                    members,
                    span: 0, // todo
                },
            )
        };

        handle
    }
}

/// todo: how do we know the address space?
pub fn access_struct_field<T: ShaderType, U: ShaderType>(
    function_builder: &mut FunctionBuilder,
    base: ExpressionHandle<T>,
    index: usize,
) -> Result<DeferredDereference<U, address_space::Private>, BuilderError> {
    let field_map = function_builder
        .module_builder
        .struct_fields
        .get(&base.type_id());

    let base = base.get_handle();

    let handle = match (field_map, base) {
        (Some(field_map), Some(base)) => {
            field_map
                .get(index)
                .expect("field index out of bounds")
                .map(|index| {
                    function_builder.add_expression(Expression::AccessIndex { base, index })
                })
                .transpose()?
        }
        _ => None,
    };

    let handle = handle.unwrap_or_else(|| ExpressionHandle::from_empty());

    Ok(DeferredDereference::new(handle))
}

pub trait Compose {
    fn compose(&self, function_builder: &mut FunctionBuilder) -> Result<ExpressionHandle<Self>, BuilderError>;
}
