use super::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Kast;

macro_rules! value {
    ($m:ident) => {
        <modules::$m::Module as core::Module<Kast>>::Value
    };
}

macro_rules! ir {
    ($m:ident) => {
        <modules::$m::Module as core::Module<Kast>>::Ir
    };
}

macro_rules! interpreter_context {
    ($m:ident) => {
        <modules::$m::Module as core::Module<Kast>>::InterpreterContext
    };
}

macro_rules! combine_modules {
    ($($m:ident),*$(,)?) => {
        pub enum Ir {
            $($m(ir!($m))),*
        }

        impl core::Ir<Kast> for Ir {
            fn eval(ir: &Self, cx: &mut InterpreterContext) -> Value {
                match ir {
                    $(Ir::$m(ir) => <ir!($m) as core::Ir<Kast>>::eval(ir, cx)),*
                }
            }
        }

        $(impl HasVariant<ir!($m)> for Ir {
            fn from_variant(variant: ir!($m)) -> Self {
                Self::$m(variant)
            }

            fn into_variant(self) -> Option<ir!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }

            fn as_variant(&self) -> Option<&ir!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }

            fn as_variant_mut(&mut self) -> Option<&mut ir!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }
        })*

        #[derive(Debug)]
        pub enum Value {
            $($m(value!($m))),*
        }

        impl core::Value<Kast> for Value {
        }

        $(impl HasVariant<value!($m)> for Value {
            fn from_variant(variant: value!($m)) -> Self {
                Self::$m(variant)
            }

            fn into_variant(self) -> Option<value!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }

            fn as_variant(&self) -> Option<&value!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }

            fn as_variant_mut(&mut self) -> Option<&mut value!($m)> {
                match self {
                    Self::$m(variant) => Some(variant),
                    _ => None,
                }
            }
        })*

        pub struct InterpreterContext {
            $($m: interpreter_context!($m)),*
        }

        impl core::InterpreterContext<Kast> for InterpreterContext {
            fn new() -> Self {
                Self {
                    $($m: <interpreter_context!($m) as core::InterpreterContext<Kast>>::new()),*
                }
            }
        }

        $(impl HasField<interpreter_context!($m)> for InterpreterContext {
            fn get_field(&self) -> &interpreter_context!($m) {
                &self.$m
            }
            fn get_field_mut(&mut self) -> &mut interpreter_context!($m) {
                &mut self.$m
            }
        })*

        impl core::Kast for Kast {
            type Value = Value;
            type Ir = Ir;
            type InterpreterContext = InterpreterContext;
            type Error = Box<dyn std::error::Error>;
        }

        $(impl modules::$m::Kast for Kast {})*
    };
}

combine_modules!(number, list);
