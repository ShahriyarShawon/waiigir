use crate::evaluator::NULL_OBJ;
use crate::object::Object;

pub fn get_builtin(fname: &str) -> Option<Object> {
    match fname {
        "puts" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            for arg in args {
                println!("{}", arg);
            }

            NULL_OBJ
        })),
        "len" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            if args.len() != 1 {
                return Object::new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }

            match &args[0] {
                Object::String(so) => Object::Integer(so.len() as i64),
                Object::Array(ao) => Object::Integer(ao.len() as i64),
                _ => Object::new_error(format!(
                    "argument to `len` not supported, got {}",
                    args[0].type_name()
                )),
            }
        })),
        "first" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            if args.len() != 1 {
                return Object::new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }

            match &args[0] {
                Object::Array(ao) => {
                    if ao.is_empty() {
                        NULL_OBJ
                    } else {
                        ao[0].clone()
                    }
                }
                other => Object::new_error(format!(
                    "argument to `first` must be ARRAY, got {}",
                    other.type_name()
                )),
            }
        })),
        "last" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            if args.len() != 1 {
                return Object::new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }
            match &args[0] {
                Object::Array(ao) => match ao.last() {
                    Some(v) => v.clone(),
                    None => NULL_OBJ,
                },
                other => Object::new_error(format!(
                    "argument to `last` must be ARRAY, got {}",
                    other.type_name()
                )),
            }
        })),
        "rest" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            if args.len() != 1 {
                return Object::new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }

            match &args[0] {
                Object::Array(ao) => match ao.len() {
                    0 => NULL_OBJ,
                    1 => Object::Array(Vec::new()),
                    _ => Object::Array(ao[1..].to_vec()),
                },
                other => Object::new_error(format!(
                    "argument to `rest` must be ARRAY, got {}",
                    other.type_name()
                )),
            }
        })),
        "push" => Some(Object::BuiltInFunction(|args: Vec<Object>| -> Object {
            if args.len() != 2 {
                return Object::new_error(format!(
                    "wrong number of arguments. got={}, want=2: {:?}",
                    args.len(),
                    args
                ));
            }

            match &args[0] {
                Object::Array(ao) => {
                    let mut new_elements = ao.clone();
                    new_elements.push(args[1].clone());
                    Object::Array(new_elements)
                }
                _ => Object::new_error(format!(
                    "first argument to `push` must be ARRAY, got {}",
                    args[0].type_name()
                )),
            }
        })),
        _ => None,
    }
}
