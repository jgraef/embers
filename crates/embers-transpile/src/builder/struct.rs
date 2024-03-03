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
    Span,
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
        AddressSpace,
        AsPointer,
        Pointer,
    },
    r#type::{
        ShaderType,
        TypeHandle,
    },
};

#[derive(ConstParamTy, Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldAccessor {
    Unnamed(usize),
    Named(&'static str),
}

pub trait FieldAccess<const FIELD: FieldAccessor> {
    const INDEX: usize;
    type Type;
}

pub trait Composable<A>: ShaderType {
    fn compose(
        values: A,
        function_generator: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<Self>, BuilderError>;
}

#[derive(Debug)]
struct StructField {
    name: Option<String>,
    ty: Handle<Type>,
}

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    type_id: TypeId,
    name: String,
    fields: Vec<StructField>,
    field_index: u32,
    field_map: Vec<Option<u32>>,
}

impl<'a> StructBuilder<'a> {
    pub fn new<T: 'static>(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            type_id: TypeId::of::<T>(),
            name: name.to_string(),
            fields: vec![],
            field_index: 0,
            field_map: vec![],
        }
    }

    pub fn add_named_field<T: ShaderType>(&mut self, name: impl ToString) {
        self.add_field::<T>(Some(name.to_string()));
    }

    pub fn add_unnamed_field<T: ShaderType>(&mut self) {
        self.add_field::<T>(None);
    }

    pub fn add_field<T: ShaderType>(&mut self, name: Option<String>) {
        let field_type = self.module_builder.get_type_by_id_or_add_it::<T>();
        if let Some(ty) = field_type.get_type() {
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
    }

    pub fn build(self) -> TypeHandle {
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
            TypeHandle::Empty
        }
        else {
            self.module_builder
                .struct_fields
                .insert(self.type_id, self.field_map);

            let handle = self.module_builder.types.insert(
                naga::Type {
                    name: Some(self.name.clone()),
                    inner: naga::TypeInner::Struct {
                        members,
                        span: 0, // todo
                    },
                },
                Span::default(),
            );

            TypeHandle::Type(handle)
        };

        self.module_builder.by_type_id.insert(self.type_id, handle);

        handle
    }
}

pub struct Field<T, U> {
    base: ExpressionHandle<T>,
    index: usize,
    _field_ty: PhantomData<U>,
}

impl<T, U> Field<T, U> {
    pub fn new<const FIELD: FieldAccessor>(base: ExpressionHandle<T>) -> Self
    where
        T: FieldAccess<FIELD, Type = U>,
    {
        Self {
            base,
            index: T::INDEX,
            _field_ty: PhantomData,
        }
    }
}

impl<T: ShaderType, U: ShaderType> AsExpression<U> for Field<T, U> {
    fn as_expression(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<ExpressionHandle<U>, BuilderError> {
        let pointer = self.as_pointer(function_builder)?;
        let expr = pointer.load(function_builder)?;
        Ok(expr)
    }
}

impl<T: ShaderType, U: ShaderType> AsPointer for Field<T, U> {
    // fixme
    type Pointer = ExpressionHandle<Pointer<U, { AddressSpace::Function }>>;

    fn as_pointer(
        &self,
        function_builder: &mut FunctionBuilder,
    ) -> Result<Self::Pointer, BuilderError> {
        let field_map = function_builder
            .module_builder
            .struct_fields
            .get(&self.base.type_id());
        let base = self.base.get_handle();

        let handle = match (field_map, base) {
            (Some(field_map), Some(base)) => {
                field_map
                    .get(self.index)
                    .expect("field index out of bounds")
                    .map(|index| {
                        function_builder.add_expression(Expression::AccessIndex { base, index })
                    })
            }
            _ => None,
        };

        let handle = handle.unwrap_or_else(|| ExpressionHandle::from_empty());

        Ok(handle)
    }
}
