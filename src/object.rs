#[derive(Debug)]
pub enum Object {
    Integer(IntegerObject),
    Boolean(BooleanObject),
    Return(ReturnValue),
    Null(NullObject),
}

#[allow(non_snake_case)]
impl Object {
    pub fn Inspect(&self) -> String {
        match self {
            Object::Integer(io) => format!("{}", io.value),
            Object::Boolean(bo) => format!("{}", bo.value),
            Object::Return(ro) => format!("{:?}", ro.value),
            Object::Null(_) => String::from("null"),
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
pub struct ReturnValue{
    pub value: Box<Object>
}

#[derive(Debug)]
pub struct NullObject {}
