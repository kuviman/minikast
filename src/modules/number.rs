use super::*;

pub enum Ir<K: Kast> {
    Const(Value),
    Add(Box<K::Ir>, Box<K::Ir>),
}

impl<K: Kast> core::Ir<K> for Ir<K> {
    fn eval(ir: &Self, cx: &mut K::InterpreterContext) -> K::Value {
        match ir {
            Ir::Const(value) => value.clone().into_enum(),
            Ir::Add(a, b) => {
                let a = K::eval(a, cx).into_variant().expect("expected a number");
                let b = K::eval(b, cx).into_variant().expect("expected a number");
                let result = match (a, b) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                    _ => panic!("can only add same types"),
                };
                result.into_enum()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i32),
    Float(f64),
}

impl<K: Kast> core::Value<K> for Value {}

pub struct InterpreterContext;

impl<K: Kast> core::InterpreterContext<K> for InterpreterContext {
    fn new() -> Self {
        Self
    }
}

pub trait Kast:
    core::Kast<
    Ir: HasVariant<Ir<Self>>,
    Value: HasVariant<Value>,
    InterpreterContext: HasField<InterpreterContext>,
>
{
}

pub struct Module;

impl<K: Kast> core::Module<K> for Module {
    type Ir = Ir<K>;
    type Value = Value;
    type InterpreterContext = InterpreterContext;
}
