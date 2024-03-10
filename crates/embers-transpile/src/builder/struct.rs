use std::any::TypeId;

use naga::{
    Expression,
    Handle,
    StructMember,
    Type,
};

use super::{
    block::BlockBuilder,
    error::BuilderError,
    expression::{
        AsExpression,
        ExpressionHandle,
    },
    module::ModuleBuilder,
    pointer::{
        address_space,
        DeferredDereference,
    },
    r#type::{
        AlignTo,
        ShaderType,
        TypeHandle,
        Width,
    },
};

pub trait Compose: Sized {
    fn compose(
        &self,
        block_builder: &mut BlockBuilder,
    ) -> Result<ExpressionHandle<Self>, BuilderError>;
}

pub trait FieldAccessor {}

pub struct UnnamedFieldAccessor<const INDEX: u32>;
impl<const INDEX: u32> FieldAccessor for UnnamedFieldAccessor<{ INDEX }> {}

pub struct NamedFieldAccessor<const NAME: &'static str>;
impl<const NAME: &'static str> FieldAccessor for NamedFieldAccessor<{ NAME }> {}

pub trait FieldAccess<F: FieldAccessor>: Sized {
    type Type;
    type Result: AsExpression<Self::Type>;

    fn access(
        block_builder: &mut BlockBuilder,
        base: ExpressionHandle<Self>,
    ) -> Result<Self::Result, BuilderError>;
}

#[derive(Clone, Debug)]
pub(super) struct StructField {
    pub name: Option<String>,
    pub ty: Handle<Type>,
    pub alignment: u32,
    pub width: u32,
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

    pub fn add_named_field<T: ShaderType + Width + AlignTo>(
        &mut self,
        name: impl ToString,
    ) -> Result<(), BuilderError> {
        self.add_field::<T>(Some(name.to_string()))?;
        Ok(())
    }

    pub fn add_unnamed_field<T: ShaderType + Width + AlignTo>(
        &mut self,
    ) -> Result<(), BuilderError> {
        self.add_field::<T>(None)?;
        Ok(())
    }

    pub fn add_field<T: ShaderType + Width + AlignTo>(
        &mut self,
        name: Option<String>,
    ) -> Result<(), BuilderError> {
        let field_type = self.module_builder.get_type_by_id_or_add_it::<T>()?;
        if let Some(ty) = field_type.get_data() {
            self.fields.push(StructField {
                name: name.clone(),
                ty,
                alignment: T::ALIGN_TO,
                width: T::WIDTH,
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
        let (members, span) = layout_struct_members(self.fields);

        let handle = if members.is_empty() {
            self.module_builder.add_empty_type::<T>()
        }
        else {
            self.module_builder
                .struct_fields
                .insert(TypeId::of::<T>(), self.field_map);

            self.module_builder
                .add_naga_type::<T>(Some(self.name), naga::TypeInner::Struct { members, span })
        };

        handle
    }
}

pub(super) fn align_to(mut address: u32, alignment: u32) -> u32 {
    let unaligned_by = address % alignment;
    if unaligned_by > 0 {
        address += alignment - unaligned_by;
    }
    address
}

pub(super) fn layout_struct_members(
    fields: impl IntoIterator<Item = StructField>,
) -> (Vec<StructMember>, u32) {
    let mut offset = 0;

    let members = fields
        .into_iter()
        .map(|field| {
            offset = align_to(offset, field.alignment);
            let member = StructMember {
                name: field.name.clone(),
                ty: field.ty,
                binding: None,
                offset,
            };
            offset += field.width;
            member
        })
        .collect();

    (members, offset)
}

/// todo: how do we know the address space?
pub fn access_struct_field<T: ShaderType, U: ShaderType>(
    block_builder: &mut BlockBuilder,
    base: ExpressionHandle<T>,
    index: u32,
) -> Result<DeferredDereference<U, address_space::Private>, BuilderError> {
    let field_map = block_builder
        .function_builder
        .module_builder
        .struct_fields
        .get(&base.type_id());

    let base = base.get_naga();

    let handle = match (field_map, base) {
        (Some(field_map), Some(base)) => {
            field_map
                .get(index as usize)
                .expect("field index out of bounds")
                .map(|index| {
                    block_builder
                        .function_builder
                        .add_expression(Expression::AccessIndex { base, index })
                })
                .transpose()?
        }
        _ => None,
    };

    let handle = handle.unwrap_or_else(|| ExpressionHandle::empty());

    Ok(DeferredDereference::new(handle))
}
