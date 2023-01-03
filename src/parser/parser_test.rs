#![allow(dead_code, unused_imports)]

#[cfg(test)]
use core::panic;
use crate::{lexer::Lexer, ast::{Node,Statement, ExpressionStatement, IfExpression}};
use super::*;
use crate::utill::*;


#[test]
fn test_let_statements() {
	let tests =
    [
		("let x = 5;", "x", 5.cover()),
		("let y = true;", "y", true.cover()),
		("let foobar = y;", "foobar", "y".to_string().cover()),
	];

	for (input, ident, val) in tests{
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		if program.statements.len() != 1 {
			panic!("program.Statements does not contain 3 statements. got={}",
				program.statements.len());
		}
		if !test_let_statement(&program.statements[0], ident.to_string(), val) {
			return
		}
	}
}
	fn test_let_statement(s: &ast::Statement, name: String, val: Literal) -> bool {
		if s.token_literal() != "let" {
			println!("s.TokenLiteral not 'let'. got={}", s.token_literal());
			return false
		}

		match s {
			Statement::LetStatement(let_stmt) => {
				if let_stmt.name.value != name {
					println!("let_stmt.Name.Value not {}. got={}", name, let_stmt.name.value);
					return false
				}
				if let_stmt.name.token_literal() != name {
					println!("s.Name not {}. got={}", name, let_stmt.name.value);
					return false
				}
				if !test_literal_expression(&let_stmt.value, &val){
					return false
				}
				return true
			}
			_ => {
				println!("s not *ast.LetStatement");
				return false
			}
		}
	}

#[test]
fn test_return_statements() {
	let tests = [
		("return 5;", 5.cover()),
		("return true;", true.cover()),
		("return foobar;", "foobar".to_string().cover()),
	];

	for (input, val) in tests{
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		if program.statements.len() != 1 {
			panic!("program.Statements does not contain 1 statements. got={}",
				program.statements.len());
		}
		for stmt in program.statements {
			if stmt.token_literal() != "return" {
				println!("s.TokenLiteral not 'return'. got={}", stmt.token_literal());
				return
			}
			match stmt {
				Statement::ReturnStatement(rs) => 
					if test_literal_expression(&rs.return_value, &val){
						return
					}
				_ => println!("s not *ast.ReturnStatement")
			}
		}
	}
}

#[test]
fn test_identifier_expression() {
	let input = "foobar;";
	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		println!("program has not enough statements. got={}",
			program.statements.len())
	}

	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::Identifier(ident) => {
					if ident.value != "foobar" {
						println!("ident.Value not {}. got={}", "foobar", ident.value)
					}
					if es.expression.token_literal() != "foobar" {
						println!("ident.TokenLiteral not {}. got={}", "foobar",
							ident.token_literal())
					}
				}
				_ => println!("exp not *ast.Identifier.")
			}
		} 
		_ => println!("program.Statements[0] is not ast.ExpressionStatement.")
	}
}

#[test]
fn test_integer_literal_expression() {
	let input = "5;";
	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		println!("program has not enough statements. got={}",
			program.statements.len())
	}

	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::IntegerLiteral(literal) => {
					if literal.value != 5 {
						println!("literal.Value not {}. got={}", 5, literal.value)
					}
					if es.expression.token_literal() != "5" {
						println!("literal.TokenLiteral not {}. got={}", "5",
							es.expression.token_literal())
					}
				}
				_ => println!("exp not *ast.IntegerLiteral.")
			}
		} 
		_ => println!("program.Statements[0] is not ast.ExpressionStatement.")
	}
}

#[test]
fn test_boolean_expression() {
	let tests = [
		("true;", true),
		("false;", false),
	];

	for (input, expected) in tests {
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		if program.statements.len() != 1 {
			panic!("program has not enough statements. got={}",
				program.statements.len());
		}
		match &program.statements[0] {
			Statement::ExpressionStatement(es) =>{
				match &es.expression {
					Expression::BooleanLiteral(bl) => {
						if bl.value != expected {
							panic!("boolean.Value not {}. got={}", expected,
							bl.value)
						}
					},
					_ => panic!("exp not *ast.Boolean. ")
				}
			},
			_ => panic!("program.Statements[0] is not ast.ExpressionStatement")
		}

	}
}

#[test]
fn test_parsing_prefix_expression() {
	let prefix_tests = [
		("!5;", "!", 5.cover()),
		("-15;", "-", 15.cover()),
		("!foobar;", "!", "foobar".to_string().cover()),
		("-foobar;", "-", "foobar".to_string().cover()),
		("!true;", "!", true.cover()),
		("!false;", "!", false.cover()),
	];

	for (input, operator, value) in prefix_tests{
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		if program.statements.len() != 1 {
			panic!("program.Statements does not contain {} statements. got={}\n",
				1, program.statements.len())
		}
		match &program.statements[0] {
			Statement::ExpressionStatement(es) => {
				match &es.expression {
					Expression::PrefixExpression(pe) => {
						if pe.operator != operator {
							panic!("exp.Operator is not '{}'. got={}",
								operator, pe.operator)
						}
						if !test_literal_expression(&pe.right, &value) {
							return
						}
					}
					_ => panic!("stmt is not ast.PrefixExpression.")
				}
			}
			_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
		} 
	}
}

#[test]
fn test_parsing_infix_expression() {
	let infix_tests:[(&str, Literal, &str, Literal); 19] = [
		("5 + 5;", 5.cover(), "+", 5.cover()),
		("5 - 5;", 5.cover(), "-", 5.cover()),
		("5 * 5;", 5.cover(), "*", 5.cover()),
		("5 / 5;", 5.cover(), "/", 5.cover()),
		("5 > 5;", 5.cover(), ">", 5.cover()),
		("5 < 5;", 5.cover(), "<", 5.cover()),
		("5 == 5;", 5.cover(), "==", 5.cover()),
		("5 != 5;", 5.cover(), "!=", 5.cover()),
		("foobar + barfoo;", "foobar".to_string().cover(), "+", "barfoo".to_string().cover()),
		("foobar - barfoo;", "foobar".to_string().cover(), "-", "barfoo".to_string().cover()),
		("foobar * barfoo;", "foobar".to_string().cover(), "*", "barfoo".to_string().cover()),
		("foobar / barfoo;", "foobar".to_string().cover(), "/", "barfoo".to_string().cover()),
		("foobar > barfoo;", "foobar".to_string().cover(), ">", "barfoo".to_string().cover()),
		("foobar < barfoo;", "foobar".to_string().cover(), "<", "barfoo".to_string().cover()),
		("foobar == barfoo;", "foobar".to_string().cover(), "==", "barfoo".to_string().cover()),
		("foobar != barfoo;", "foobar".to_string().cover(), "!=", "barfoo".to_string().cover()),
		("true == true", true.cover(), "==", true.cover()),
		("true != false", true.cover(), "!=", false.cover()),
		("false == false", false.cover(), "==", false.cover()),
	];

	for (input, left, operator, right) in infix_tests {
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		if program.statements.len() != 1 {
			println!("program.Statements does not contain {} statements. got={}\n",
				1, program.statements.len())
		}
		match &program.statements[0] {
			Statement::ExpressionStatement(es) => {
				if !test_infix_expression(&es.expression, left, operator.to_string(), right){
					return
				}
			}
			_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
		}


	}
}
	fn test_infix_expression(exp: &Expression, left: Literal, operator: String, right: Literal) -> bool {
		match exp {
			Expression::InfixExpression(ie) => {
				if !test_literal_expression(&ie.left, &left) {
					return false
				}
			
				if ie.operator != operator {
					println!("exp.Operator is not '{}'.", operator);
					return false
				}
			
				if !test_literal_expression(&ie.right, &right) {
					return false
				}
			}
			_ => {
				println!("exp is not ast.OperatorExpression.");
				return false
			}
		}
		return true
	}

#[test]
fn test_operator_precedence_parsing() {
	let tests = [
		[
			"-a * b",
			"((-a) * b)",
		],
		[
			"!-a",
			"(!(-a))",
		],
		[
			"a + b + c",
			"((a + b) + c)",
		],
		[
			"a + b - c",
			"((a + b) - c)",
		],
		[
			"a * b * c",
			"((a * b) * c)",
		],
		[
			"a * b / c",
			"((a * b) / c)",
		],
		[
			"a + b / c",
			"(a + (b / c))",
		],
		[
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		],
		[
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		],
		[
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		],
		[
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		],
		[
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		],
		[
			"true",
			"true",
		],
		[
			"false",
			"false",
		],
		[
			"3 > 5 == false",
			"((3 > 5) == false)",
		],
		[
			"3 < 5 == true",
			"((3 < 5) == true)",
		],
		[
			"1 + (2 + 3) + 4",
			"((1 + (2 + 3)) + 4)",
		],
		[
			"(5 + 5) * 2",
			"((5 + 5) * 2)",
		],
		[
			"2 / (5 + 5)",
			"(2 / (5 + 5))",
		],
		[
			"(5 + 5) * 2 * (5 + 5)",
			"(((5 + 5) * 2) * (5 + 5))",
		],
		[
			"-(5 + 5)",
			"(-(5 + 5))",
		],
		[
			"!(true == true)",
			"(!(true == true))",
		],
		[
			"a + add(b * c) + d",
			"((a + add((b * c))) + d)",
		],
		[
			"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		],
		[
			"add(a + b + c * d / f + g)",
			"add((((a + b) + ((c * d) / f)) + g))",
		],
	];

	for [input, expected] in tests {
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);

		let actual = program.string();
		if actual != expected {
			panic!("expected={}, got={}", expected, actual);
		}
	}
}

#[test]
fn test_if_expression() {
	let input = "if (x < y) { x }";

	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		panic!("program.Body does not contain {} statements. got={}\n",
			1, program.statements.len());
	}
	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::IfExpression(ie) => {
					if !test_infix_expression(&ie.condition, "x".to_string().cover(), "<".to_string(), "y".to_string().cover()) {
						return
					}
					if ie.consequence.statements.len() != 1 {
						panic!("consequence is not 1 statements. got={}\n",
						ie.consequence.statements.len())
					}
					match &ie.consequence.statements[0] {
						Statement::ExpressionStatement(es) => {
							if !test_identifier(&es.expression, "x".to_string()){
								return
							}
						}
						_ => panic!("Statements[0] is not ast.ExpressionStatement.")
					}
					match &ie.alternative {
						Some(_) => panic!("exp.Alternative.Statements was not nil."),
						None => (),
					}
				}
				_ => panic!("stmt.Expression is not ast.IfExpression.")
			}
		}
		_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
	}
}

#[test]
fn test_if_else_expression() {
	let input = "if (x < y) { x } else { y }";
	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		panic!("program.Body does not contain {} statements. got={}\n",
			1, program.statements.len());
	}
	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::IfExpression(ie) => {
					if !test_infix_expression(&ie.condition, "x".to_string().cover(), "<".to_string(), "y".to_string().cover()) {
						return
					}
					if ie.consequence.statements.len() != 1 {
						panic!("consequence is not 1 statements. got={}\n",
						ie.consequence.statements.len())
					}
					match &ie.consequence.statements[0] {
						Statement::ExpressionStatement(es) => {
							if !test_identifier(&es.expression, "x".to_string()){
								return
							}
						}
						_ => panic!("Statements[0] is not ast.ExpressionStatement.")
					}
					let alt;
					match &ie.alternative {
						None => return,
						Some(x) => alt = x,
					}
					if alt.statements.len() != 1 {
						panic! ("exp.Alternative.Statements does not contain 1 statements. got={}\n",
						alt.statements.len())
					}
					match &alt.statements[0] {
						Statement::ExpressionStatement(es) => {
							if !test_identifier(&es.expression, "y".to_string()){
								return
							}
						}
						_ => panic!("Statements[0] is not ast.ExpressionStatement.")
					}
				}
				_ => panic!("stmt.Expression is not ast.IfExpression.")
			}
		}
		_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
	}

}

#[test]
fn test_function_literal_parsing() {
	let input = "fn(x, y) { x + y; }";

	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		panic!("program.Body does not contain {} statements. got={}\n",
			1, program.statements.len())
	}
	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::FunctionLiteral(fl) => {
					if fl.parameters.len() != 2 {
						panic!("function literal parameters wrong. want 2, got={}\n",
						fl.parameters.len())
					}
					test_identifier2(&fl.parameters[0], "x".to_string());
					test_identifier2(&fl.parameters[1], "y".to_string());
					if fl.body.statements.len() != 1{
						panic!("function.Body.Statements has not 1 statements. got={}\n",
						fl.body.statements.len())
					}
					match &fl.body.statements[0] {
						Statement::ExpressionStatement(es) => {
							test_infix_expression(&es.expression, "x".to_string().cover(), "+".to_string(), "y".to_string().cover());
						}
						_ => panic!("function body stmt is not ast.ExpressionStatement.")
					}
				}
				_ => panic!("stmt.Expression is not ast.FunctionLiteral.")
			}
		}
		_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
	}
}

#[test]
fn test_function_parameter_parsing() {
	let tests = [
		("fn() {};", vec![]),
		("fn(x) {};", vec!["x"]),
		("fn(x, y, z) {};", vec!["x", "y", "z"]),
	];

	for (input, expect) in tests {
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);
		match &program.statements[0] {
			Statement::ExpressionStatement(es) => {
				match &es.expression {
					Expression::FunctionLiteral(fl) => {
						if fl.parameters.len() != expect.len() {
							panic!("length parameters wrong. want {}, got={}\n",
							expect.len(),fl.parameters.len())
						}
						for (i, ident) in expect.iter().enumerate() {
							test_identifier2(&fl.parameters[i], ident.to_string());
						}
					}
					_ => panic!("")
				}
			}
			_ => panic!("")
		}
	}
}

#[test]
fn test_call_expression_parsing() {
	let input = "add(1, 2 * 3, 4 + 5);";

	let l = lexer::Lexer::new(input.to_string());
	let mut p = Parser::new(l);
	let program = p.parse_program();
	check_parser_error(&p);

	if program.statements.len() != 1 {
		panic!("program.Body does not contain {} statements. got={}\n",
			1, program.statements.len())
	}
	match &program.statements[0] {
		Statement::ExpressionStatement(es) => {
			match &es.expression {
				Expression::CallExpression(ce) => {
					if test_identifier(&ce.function, "add".to_string()){
						return
					}
					if ce.arguments.len() != 3{
						panic!("wrong length of arguments. got={}", ce.arguments.len())
					}
					test_literal_expression(&ce.arguments[0], &1.cover());
					test_infix_expression(&ce.arguments[1], 2.cover(), "*".to_string(), 3.cover());
					test_infix_expression(&ce.arguments[2], 4.cover(), "+".to_string(), 5.cover());
				}
				_ => panic!("stmt.Expression is not ast.CallExpression.")
			}
		}
		_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
	}
}

#[test]
fn test_call_expression_parameter_parsing() {
	let tests = [
		(
			"add();",
			"add",
			vec![]
		),
		(
			"add(1);",
			"add",
			vec!["1"]
		),
		(
			"add(1, 2 * 3, 4 + 5);",
			"add",
			vec!["1", "(2 * 3)", "(4 + 5)"]
		)
	];

	for (input, expected_ident, expected_args) in tests {
		let l = lexer::Lexer::new(input.to_string());
		let mut p = Parser::new(l);
		let program = p.parse_program();
		check_parser_error(&p);
		match &program.statements[0] {
			Statement::ExpressionStatement(es) => {
				match &es.expression {
					Expression::CallExpression(ce) => {
						if !test_identifier(&ce.function, expected_ident.to_string()) {
							return
						}
						if ce.arguments.len() != expected_args.len(){
							panic!("wrong number of arguments want={}, got={}",
							expected_args.len(), ce.arguments.len())
						}
						for (i, arg) in expected_args.iter().enumerate(){
							if ce.arguments[i].string() != *arg{
								panic!("argument {} wrong. want={}, got={}", i,
								arg, ce.arguments[i].string())
							}
						}
					}
					_ => panic!("stmt.Expression is not ast.CallExpression.")
				}
			}
			_ => panic!("program.Statements[0] is not ast.ExpressionStatement.")
		}
	}
}


fn test_literal_expression(exp: &Expression, expected: &Literal) -> bool {
	match expected{
		Literal::Int(i) => {
			return test_integer_literal(&exp, *i);
		},
		Literal::String(s) => {
			return test_identifier(exp, s.to_string());
		},
		Literal::Bool(b) => {
			return test_boolean_literal(exp, *b);
		}
	}
}
	fn test_integer_literal(e: &Expression, value: i32) -> bool {
		match &e {
			Expression::IntegerLiteral(il) => {
				if il.value != value {
					println!("integ.Value not {}. got={}", value, il.value);
					return false
				}
			
				if e.token_literal() != value.to_string() {
					println!("integ.TokenLiteral not {}. got={}", value,
						e.token_literal());
					return false
				}
				return true
			}
			_ => {
				println!("il not *ast.IntegerLiteral");
				return false
			}
		}
	}
	fn test_boolean_literal(e: &Expression, value: bool) -> bool {
		match &e {
			Expression::BooleanLiteral(bl) => {
				if bl.value != value {
					println!("bool.Value not {}. got={}", value, bl.value);
					return false
				}
			
				if e.token_literal() != value.to_string() {
					println!("bool.TokenLiteral not {}. got={}", value,
						e.token_literal());
					return false
				}
				return true
			}
			_ => {
				println!("il not *ast.BooleanLiteral");
				return false
			}
		}
	}
	fn test_identifier(exp: &Expression, value: String) -> bool {
		match exp {
			Expression::Identifier(ie) => {
				if ie.value != value {
					println!("ident.Value not {}. got={}", value, ie.value);
					return false
				}
				if ie.token_literal() != value {
					println!("ident.TokenLiteral not {}. got={}", value,
					ie.token_literal());
					return false
				}
			}
			_ => {
				println!("exp not *ast.Identifier.");
				return false
			}
		}
		return true
	}
	fn test_identifier2(ie: &Identifier, value: String) -> bool {
		if ie.value != value {
			println!("ident.Value not {}. got={}", value, ie.value);
			return false
		}
		if ie.token_literal() != value {
			println!("ident.TokenLiteral not {}. got={}", value,
			ie.token_literal());
			return false
		}
		return true
	}
fn check_parser_error(p: &Parser) {
	let errors = p.errors();
	if errors.len() == 0 {
		return
	}

	println!("PARSE ERROR: got several error while parsing program");
	for error in errors {
		println!("{}", error);
	}
	panic!("error end")
}
