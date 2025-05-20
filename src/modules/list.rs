use super::*;

pub enum Ir<K: Kast> {
    MakeList { elements: Vec<K::Ir> },
}

impl<K: Kast> core::Ir<K> for Ir<K> {
    fn eval(ir: &Self, cx: &mut K::InterpreterContext) -> K::Value {
        match ir {
            Ir::MakeList { elements } => Value {
                elements: elements
                    .iter()
                    .map(|element| K::eval(element, cx))
                    .collect(),
            }
            .into_enum(),
        }
    }
}

#[derive(Debug)]
pub struct Value<K: Kast> {
    elements: Vec<K::Value>,
}

impl<K: Kast> core::Value<K> for Value<K> {}

pub struct InterpreterContext;

impl<K: Kast> core::InterpreterContext<K> for InterpreterContext {
    fn new() -> Self {
        Self
    }
}

pub trait Kast:
    core::Kast<
        Ir: HasVariant<Ir<Self>>,
        Value: HasVariant<Value<Self>>,
        InterpreterContext: HasField<InterpreterContext>,
    >
{
}

pub struct Module;

impl<K: Kast> core::Module<K> for Module {
    type Ir = Ir<K>;
    type Value = Value<K>;
    type InterpreterContext = InterpreterContext;
}
