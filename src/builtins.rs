use crate::evaluator::NULL_OBJ;
use crate::object::{ArrayObject, BuiltInFunctionObject, IntegerObject, Object};

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
                    Object::Array(ao) => {
                        return Object::Integer(IntegerObject {
                            value: ao.elements.len() as i64,
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
        "first" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            function: |args: Vec<Object>| -> Object {
                if args.len() != 1 {
                    return Object::new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args.get(0).expect("hope so").type_name() != "ARRAY" {
                    return Object::new_error(format!(
                        "argument to `first` must be ARRAY, got {}",
                        args.get(0).expect("hopesp").type_name()
                    ));
                }

                match &args[0] {
                    Object::Array(ao) => {
                        if ao.elements.len() == 0 {
                            NULL_OBJ
                        } else {
                            ao.elements[0].clone()
                        }
                    }
                    _ => NULL_OBJ,
                }
            },
        })),
        "last" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            function: |args: Vec<Object>| -> Object {
                if args.len() != 1 {
                    return Object::new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args.get(0).expect("hope so").type_name() != "ARRAY" {
                    return Object::new_error(format!(
                        "argument to `last` must be ARRAY, got {}",
                        args.first().expect("hopesp").type_name()
                    ));
                }

                match &args[0] {
                    Object::Array(ao) => match ao.elements.last() {
                        Some(v) => v.clone(),
                        None => NULL_OBJ,
                    },
                    _ => NULL_OBJ,
                }
            },
        })),
        "rest" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            function: |args: Vec<Object>| -> Object {
                if args.len() != 1 {
                    return Object::new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args.get(0).expect("hope so").type_name() != "ARRAY" {
                    return Object::new_error(format!(
                        "argument to `rest` must be ARRAY, got {}",
                        args.first().expect("hopesp").type_name()
                    ));
                }

                match &args[0] {
                    Object::Array(ao) => match ao.elements.len() {
                        0 => NULL_OBJ,
                        1 => Object::Array(ArrayObject {
                            elements: Vec::new(),
                        }),
                        _ => Object::Array(ArrayObject {
                            elements: ao.elements.clone()[1..].to_vec(),
                        }),
                    },
                    _ => NULL_OBJ,
                }
            },
        })),
        "push" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            function: |args: Vec<Object>| -> Object {
                if args.len() != 2 {
                    return Object::new_error(format!(
                        "wrong number of arguments. got={}, want=2: {:?}",
                        args.len(),
                        args
                    ));
                }

                match &args[0] {
                    Object::Array(ao) => {
                        let mut new_elements = ao.elements.clone();
                        new_elements.push(args[1].clone());
                        Object::Array(ArrayObject {
                            elements: new_elements,
                        })
                    }
                    _ => Object::new_error(format!(
                        "first argument to `push` must be ARRAY, got {}",
                        args[0].type_name()
                    )),
                }
            },
        })),
        _ => {
            eprintln!("yo wtf is {}", fname);
            None
        }
    }
}
