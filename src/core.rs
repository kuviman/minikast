use super::*;

pub trait Kast: Sized + std::fmt::Debug + Clone + Copy {
    type Value: Value<Self>;
    type Ir: Ir<Self>;
    type InterpreterContext: InterpreterContext<Self>;
    type Error;

    fn eval(ir: &Self::Ir, cx: &mut Self::InterpreterContext) -> Self::Value {
        <Self::Ir as Ir<Self>>::eval(ir, cx)
    }
}

pub trait Ir<K: Kast> {
    fn eval(ir: &Self, cx: &mut K::InterpreterContext) -> K::Value;
}

pub trait Value<K: Kast>: std::fmt::Debug {}

pub trait InterpreterContext<K: Kast> {
    fn new() -> Self;
}

pub trait Module<K: Kast> {
    type Ir: Ir<K>;
    type Value: Value<K>;
    type InterpreterContext: InterpreterContext<K>;
}
