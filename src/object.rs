pub enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}

pub trait Object {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}
