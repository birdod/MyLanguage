use crate::token::INT;


type ObjectType = String;

enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null
}

struct Integer {
    value: i32
}
struct Boolean {
    value: bool
}

impl Object {
    fn r#type(&self) -> ObjectType {  
        match self{
            Object::Integer(_) =>  INTEGER_OBJ.to_string(),
            Object::Boolean(_) =>  BOOLEAN_OBJ.to_string(),
            Object::Null =>  NULL_OBJ.to_string()

        }  
    }
    fn inspect(&self) -> String {
        match self{
            Object::Integer(i) =>  format!("{}",i.value),
            Object::Boolean(b) =>  format!("{}",b.value),
            Object::Null =>  format!("") 
        }
    }
}




pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";