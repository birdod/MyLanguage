pub enum Literal {
	Int(i32),
	String(String),
	Bool(bool)
}
pub trait Cover {
	fn cover(&self) -> Literal;
}
impl Cover for i32 {
	fn cover(&self) -> Literal {
		Literal::Int(*self)
	}
}
impl Cover for String {
	fn cover(&self) -> Literal {
		Literal::String(self.clone())
	}
}
impl Cover for bool {
	fn cover(&self) -> Literal {
		Literal::Bool(*self)
	}
}