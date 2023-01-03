

type ObjectType = String;

trait Object {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub struct Integer {
    value: i32
}
pub struct Boolean {
    value: bool
}

impl Object for Integer {
    fn r#type(&self) -> ObjectType {  
        INTEGER_OBJ.to_string()
    }
    fn inspect(&self) -> String {
        format!("{}",self.value)
    }
}
impl Object for Boolean {
    fn r#type(&self) -> ObjectType {  
        BOOLEAN_OBJ.to_string()
    }
    fn inspect(&self) -> String {
        format!("{}",self.value)
    }
}



pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";

fn main(test: &impl Object) {
    match test.r#type() {
        Integer => ()
    }
}