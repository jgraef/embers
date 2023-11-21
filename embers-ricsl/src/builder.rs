use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
};

use naga::{
    Binding,
    Expression,
    Span, Statement, Function, Block, FunctionResult,
};

use crate::{
    arena::{
        Arena,
        Handle,
    },
    RicslType,
};

#[derive(Debug)]
enum TypeInner {
    Instrinsic {
        naga: naga::Handle<naga::Type>,
    },
    Unit,
    Struct {
        naga: naga::Handle<naga::Type>,
        fields: Arena<StructField>,
        field_names: HashMap<String, Handle<StructField>>,
    },
    Func {
        naga: naga::Handle<naga::Function>,
        this: Option<Handle<Type>>,
        inputs: Vec<FunctionArgument>,
        output: Handle<Type>,
    }
}

#[derive(Debug)]
pub struct Type {
    name: String,
    inner: TypeInner,
    type_id: TypeId,
}

#[derive(Debug, Default)]
pub struct ModuleBuilder {
    types: Arena<Type>,
    by_type_id: HashMap<TypeId, Handle<Type>>,
    naga_types: naga::UniqueArena<naga::Type>,
    naga_functions: naga::Arena<naga::Function>,
}

impl ModuleBuilder {
    pub fn add_struct<T: 'static>(&mut self, name: impl ToString) -> StructBuilder {
        StructBuilder::new::<T>(self, name)
    }

    pub(crate) fn add_intrinsic_type<T: 'static>(
        &mut self,
        name: impl ToString,
        naga_type_inner: naga::TypeInner,
    ) -> Handle<Type> {
        let name = name.to_string();

        let naga_type = self.naga_types.insert(
            naga::Type {
                name: Some(name.clone()),
                inner: naga_type_inner,
            },
            naga::Span::default(),
        );

        let type_id = TypeId::of::<T>();
        let ty = Type {
            name,
            inner: TypeInner::Instrinsic { naga: naga_type },
            type_id,
        };

        self.types.insert(ty)
    }

    pub fn add_function(&mut self, name: impl ToString) -> FunctionBuilder {
        FunctionBuilder::new(self, name)
    }

    pub fn get_type_by_id_or_add_it<T: RicslType>(&mut self) -> Handle<Type> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<T>()) {
            *handle
        }
        else {
            T::add_to_module(self)
        }
    }

    pub fn get_func_by_id_or_add_it<F: 'static, G: FunctionGenerator>(&mut self, f: &F, g: G) -> Handle<Type> {
        if let Some(handle) = self.by_type_id.get(&TypeId::of::<F>()) {
            *handle
        }
        else {
            g.generate(self)
        }
    }

    fn get_naga_type(&self, handle: Handle<Type>) -> Option<naga::Handle<naga::Type>> {
        let ty = self.types.get(handle).unwrap();
        match &ty.inner {
            TypeInner::Instrinsic { naga } => Some(*naga),
            TypeInner::Struct { naga, .. } => Some(*naga),
            _ => None,
        }
    }

    fn get_naga_func(&self, handle: Handle<Type>) -> Option<naga::Handle<naga::Function>> {
        let ty = self.types.get(handle).unwrap();
        match &ty.inner {
            TypeInner::Func { naga, .. } => Some(*naga),
            _ => None
        }
    }

    pub fn build(self) -> Module {
        let naga = naga::Module {
            types: self.naga_types,
            special_types: Default::default(),
            constants: Default::default(),
            global_variables: Default::default(),
            const_expressions: Default::default(),
            functions: self.naga_functions,
            entry_points: Default::default(),
        };
        Module { naga }
    }
}

pub struct StructBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,
    type_id: TypeId,
    name: String,
    fields: Arena<StructField>,
    field_names: HashMap<String, Handle<StructField>>,
}

impl<'a> StructBuilder<'a> {
    pub fn new<T: 'static>(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            type_id: TypeId::of::<T>(),
            name: name.to_string(),
            fields: Default::default(),
            field_names: Default::default(),
        }
    }

    pub fn add_named_field<T: RicslType>(&mut self, name: impl ToString) {
        self.add_field::<T>(Some(name.to_string()));
    }

    pub fn add_unnamed_field<T: RicslType>(&mut self) {
        self.add_field::<T>(None);
    }

    pub fn add_field<T: RicslType>(&mut self, name: Option<String>) {
        let field_type = self.module_builder.get_type_by_id_or_add_it::<T>();
        let handle = self.fields.insert(StructField {
            name: name.clone(),
            ty: field_type,
        });
        if let Some(name) = name {
            self.field_names.insert(name, handle);
        }
    }

    pub fn build(self) -> Handle<Type> {
        let mut members = vec![];
        for (_, field) in self.fields.iter() {
            let Some(naga_type) = self.module_builder.get_naga_type(field.ty)
            else {
                continue;
            };
            members.push(naga::StructMember {
                name: field.name.clone(),
                ty: naga_type,
                binding: None,
                offset: 0, // todo
            });
        }

        let naga_type = self.module_builder.naga_types.insert(
            naga::Type {
                name: Some(self.name.clone()),
                inner: naga::TypeInner::Struct { members, span: 0 },
            },
            naga::Span::default(),
        );

        let ty = Type {
            name: self.name,
            inner: TypeInner::Struct {
                fields: self.fields,
                field_names: self.field_names,
                naga: naga_type,
            },
            type_id: self.type_id,
        };

        self.module_builder.types.insert(ty)
    }
}

#[derive(Debug)]
struct StructField {
    name: Option<String>,
    ty: Handle<Type>,
}

pub struct FunctionBuilder<'a> {
    pub module_builder: &'a mut ModuleBuilder,
    this: Option<Handle<Type>>,
    name: Option<String>,
    inputs: Vec<FunctionArgument>,
    output: Option<Handle<Type>>,
    expressions: naga::Arena<Expression>,
    named_expressions: HashMap<String, naga::Handle<Expression>>,
    statements: Vec<Statement>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(module_builder: &'a mut ModuleBuilder, name: impl ToString) -> Self {
        Self {
            module_builder,
            this: None,
            name: Some(name.to_string()),
            inputs: vec![],
            output: None,
            expressions: Default::default(),
            named_expressions: Default::default(),
            statements: vec![],
        }
    }
    pub fn add_input_receiver(&mut self) {
        assert_eq!(self.inputs.len(), 0);
        self.inputs.push(FunctionArgument::Receiver);
    }

    pub fn add_input_named<T: RicslType>(
        &mut self,
        ident: impl ToString,
        is_mut: bool,
        binding: Option<Binding>,
    ) -> ExpressionHandle<T> {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();

        let i = self.inputs.len() as u32;
        let expr = self
            .expressions
            .append(Expression::FunctionArgument(i), Span::default());

        self.inputs.push(FunctionArgument::Named {
            ty,
            binding,
            ident: ident.to_string(),
            is_mut,
            expr,
        });

        ExpressionHandle::from_handle(expr)
    }

    pub fn add_input_wild<T: RicslType>(&mut self) {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();

        self.inputs.push(FunctionArgument::Wild { ty });
    }

    pub fn add_output<T: RicslType>(&mut self) {
        let ty = self.module_builder.get_type_by_id_or_add_it::<T>();
        self.output = Some(ty);
    }

    pub fn add_expression<T>(&mut self, expr: Expression) -> ExpressionHandle<T> {
        let handle = self.expressions.append(expr, Default::default());
        ExpressionHandle::from_handle(handle)
    }

    pub fn name_expression(&mut self, name: impl ToString, expr: naga::Handle<Expression>) {
        self.named_expressions.insert(name.to_string(), expr);
    }

    pub fn add_call<F: 'static, G: FunctionGenerator<Return = R>, R: RicslType>(&mut self, f: &F, gen: G, args: Vec<naga::Handle<Expression>>) -> ExpressionHandle<R> {
        let type_handle = self.module_builder.get_func_by_id_or_add_it(
            f,
            gen,
        );

        //let fun_type = self.module_builder.types.get(type_handle).unwrap();
        let naga_fun = self.module_builder.get_naga_func(type_handle).unwrap();
        
        let ret = if R::PHANTOM {
            ExpressionHandle::<R>::phantom()
        }
        else {
            let expr = Expression::CallResult(naga_fun);
            self.add_expression::<R>(expr)
        };
        
        self.add_statement(
            Statement::Call {
                function: naga_fun,
                arguments: args,
                result: ret.handle(),
            }
        );

        ret
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn build(self) -> Handle<Function> {
        let arguments = vec![];
        for arg in self.inputs {
            match arg {
                FunctionArgument::Receiver => {
                    arguments.push(naga::FunctionArgument {
                        name: Some("self".to_owned()),
                        ty: self.module_builder.get_naga_type(self.this.unwrap()).unwrap(),
                        binding: None,
                    })
                },
                FunctionArgument::Named { ty, binding, ident, is_mut, expr } => {
                    arguments.push(naga::FunctionArgument {
                        name: Some(ident),
                        ty: self.module_builder.get_naga_type(ty).unwrap(),
                        binding,
                    })
                },
                FunctionArgument::Wild { ty } => {},
            }
        }

        let result = self.output.map(|handle| {
            FunctionResult {
                ty: self.module_builder.get_naga_type(handle).unwrap(),
                binding: None,
            }
        });

        let func = Function {
            name: self.name,
            arguments,
            result,
            local_variables: Default::default(),
            expressions: self.expressions,
            named_expressions: Default::default(),
            body: Block::from_vec(self.statements),
        };

        self.module_builder.naga_functions.append(func, Default::default())
    }
}

#[derive(Debug)]
enum FunctionArgument {
    Receiver,
    Named {
        ty: Handle<Type>,
        binding: Option<Binding>,
        ident: String,
        is_mut: bool,
        expr: naga::Handle<Expression>,
    },
    Wild {
        ty: Handle<Type>,
    },
}

#[must_use]
pub struct ExpressionHandle<T> {
    handle: Option<naga::Handle<Expression>>,
    _t: PhantomData<T>,
}

impl<T> ExpressionHandle<T> {
    pub fn from_handle(handle: naga::Handle<Expression>) -> Self {
        Self {
            handle: Some(handle),
            _t: PhantomData,
        }
    }

    pub fn phantom() -> Self {
        Self {
            handle: None,
            _t: PhantomData
        }
    }

    pub fn handle(&self) -> Option<naga::Handle<Expression>> {
        self.handle
    }
}

impl<T: 'static> ExpressionHandle<T> {
    pub fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}


pub trait FunctionGenerator {
    type Return;

    fn generate(&self, module_builder: &mut ModuleBuilder) -> Handle<Type>;
}


pub struct Module {
    naga: naga::Module,
}




#[derive(Debug, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    items: HashMap<String, ()>,
}

impl<'a> Scope<'a> {
    pub fn sub(&'a self) -> Self {
        Self {
            parent: Some(self),
            items: Default::default(),
        }
    }
}
