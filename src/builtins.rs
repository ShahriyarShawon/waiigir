use crate::object::{BuiltInFunctionObject, IntegerObject, Object};

pub fn get_builtin(fname: &str) -> Option<Object> {
    match fname {
        "len" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            function: |args: Vec<Object>| -> Object {
                if args.len() != 1 {
                    return Object::new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::String(so) => {
                        return Object::Integer(IntegerObject {
                            value: so.value.len() as i64,
                        });
                    }
                    _ => {
                        return Object::new_error(format!(
                            "argument to `len` not supported, got {}",
                            args[0].type_name()
                        ));
                    }
                }
            },
        })),
        _ => {
            eprintln!("yo wtf is {}", fname);
            None
        }
    }
}
