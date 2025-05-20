use super::*;

pub enum Ir {
    Literal(Value),
}

impl<K: Kast> core::Ir<K> for Ir {
    fn eval(ir: &Self, cx: &mut K::InterpreterContext) -> K::Value {
        match ir {
            Ir::Literal(value) => value.clone().into_enum(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Value(pub String);

impl<K: Kast> core::Value<K> for Value {}

pub struct InterpreterContext;

impl<K: Kast> core::InterpreterContext<K> for InterpreterContext {
    fn new() -> Self {
        Self
    }
}

pub trait Kast:
    core::Kast<
        Ir: HasVariant<Ir>,
        Value: HasVariant<Value>,
        InterpreterContext: HasField<InterpreterContext>,
    >
{
}

pub struct Module;

impl<K: Kast> core::Module<K> for Module {
    type Ir = Ir;
    type Value = Value;
    type InterpreterContext = InterpreterContext;
}
