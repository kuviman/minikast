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
    let element3 = string::Ir::Literal(string::Value("hello, world".to_owned()));
    let ir = list::Ir::MakeList {
        elements: vec![
            element1.into_enum(),
            element2.into_enum(),
            element3.into_enum(),
        ],
    };
    // ir = make_list(123, 1.0 + 2.0, "hello, world")

    let ir: kast::Ir = ir.into_enum();

    let mut cx = <<kast::Kast as core::Kast>::InterpreterContext as core::InterpreterContext<
        kast::Kast,
    >>::new();
    let value = <kast::Kast as core::Kast>::eval(&ir, &mut cx);
    println!("{value:?}");
}
