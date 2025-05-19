mod prelude;
use prelude::*;

mod core;

mod modules;
use modules::*;

mod kast;

fn main() {
    let element1 = number::Ir::Const(number::Value::Integer(123));
    let element2 = number::Ir::Add(
        number::Ir::Const(number::Value::Float(1.0)).into_enum_box(),
        number::Ir::Const(number::Value::Float(2.0)).into_enum_box(),
    );
    let ir = list::Ir::Push {
        list: list::Ir::Push {
            list: list::Ir::NewList {}.into_enum_box(),
            element: element1.into_enum_box(),
        }
        .into_enum_box(),
        element: element2.into_enum_box(),
    };
    // ir = push(push([], 123), 1.0 + 2.0)

    let ir: kast::Ir = ir.into_enum();

    let mut cx = <<kast::Kast as core::Kast>::InterpreterContext as core::InterpreterContext<
        kast::Kast,
    >>::new();
    let value = <kast::Kast as core::Kast>::eval(&ir, &mut cx);
    println!("{value:?}");
}
