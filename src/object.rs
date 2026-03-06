// pub type ObjectType = String;
#[derive(PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null
}


fn obj_map(o: &Object) -> ObjectType {
    match o {
        // Object::Integer(_) => String::from("INTEGER"),
        // Object::Boolean(_) => String::from("BOOLEAN"),
        // Object::Null(_) => String::from("NULL"),
        Object::Integer(_) => ObjectType::Integer,
        Object::Boolean(_) => ObjectType::Boolean,
        Object::Null(_) => ObjectType::Null
    }
}
#[derive(Debug)]
pub enum Object {
    Integer(IntegerObject),
    Boolean(BooleanObject),
    Null(NullObject),
}

#[allow(non_snake_case)]
impl Object {
    pub fn Type(&self) -> ObjectType {
        obj_map(self)
    }

    pub fn Inspect(&self) -> String {
        match self {
            Object::Integer(io) => return format!("{}", io.value),
            Object::Boolean(bo) => return format!("{}", bo.value),
            Object::Null(_) => return String::from("null"),
            _ => {todo!()}
        }
    }

    pub fn new_integer(val: i64) -> Object {
        Object::Integer(IntegerObject { value: val })
    }
}


#[derive(Debug)]
pub struct IntegerObject {
    pub value: i64
}

#[derive(Debug)]
pub struct BooleanObject {
    pub value: bool 
}

#[derive(Debug)]
pub struct NullObject {}
