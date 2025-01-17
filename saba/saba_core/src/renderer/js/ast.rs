use crate::renderer::js::token::JsLexer;
use crate::renderer::js::token::Token;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    body: Vec<Rc<Node>>,
}

impl Program {
    pub fn new() -> Self {
        Self { body: Vec::new() }
    }

    pub fn set_body(&mut self, body: Vec<Rc<Node>>) {
        self.body = body;
    }

    pub fn body(&self) -> &Vec<Rc<Node>> {
        &self.body
    }
}

// Construct an AST from the token stream.
pub struct JsParser {
    t: Peekable<JsLexer>,
}

impl JsParser {
    pub fn new(t: JsLexer) -> Self {
        Self { t: t.peekable() }
    }

    // p.363
    // Program ::= ( SourceElements )? <EOF>
    pub fn parse_ast(&mut self) -> Program {
        let mut program = Program::new();

        let mut body = Vec::new();

        loop {
            let node = self.source_element(); // 1

            match node {
                Some(n) => body.push(n),
                None => {
                    // 2
                    program.set_body(body);
                    return program;
                }
            }
        }
    }

    // SourceElement ::= Statement
    fn source_element(&mut self) -> Option<Rc<Node>> {
        match self.t.peek() {
            Some(t) => t,
            None => return None,
        };

        self.statement()
    }

    // Statement ::= ExpressionStatement
    // ExpressionStatement ::= AssignmentExpression ( ";" )?
    fn statement(&mut self) -> Option<Rc<Node>> {
        let node = Node::new_expression_statement(self.assignment_expression());

        if let Some(Token::Punctuator(c)) = self.t.peek() {
            // Consume a semicolon.
            if c == &';' {
                assert!(self.t.next().is_some());
            }
        }

        node
    }

    // AssignmentExpression ::= AdditiveExpression
    fn assignment_expression(&mut self) -> Option<Rc<Node>> {
        self.additive_expression()
    }

    // AdditiveExpression ::= LeftHandSideExpression ( AdditiveOperator AssignmentExpression )*
    fn additive_expression(&mut self) -> Option<Rc<Node>> {
        let left = self.left_hand_side_expression(); // 1

        let t = match self.t.peek() {
            Some(token) => token.clone(),
            None => return left, // 2
        };

        match t {
            Token::Punctuator(c) => match c {
                // 3
                '+' | '-' => {
                    // Consume an additive or subtractive operator.
                    assert!(self.t.next().is_some());
                    Node::new_additive_expression(c, left, self.assignment_expression())
                }
                _ => left,
            },
            _ => left,
        }
    }

    // LeftHandSideExpression ::= MemberExpression
    fn left_hand_side_expression(&mut self) -> Option<Rc<Node>> {
        self.member_expression()
    }

    // MemberExpression ::= PrimaryExpression
    fn member_expression(&mut self) -> Option<Rc<Node>> {
        self.primary_expression()
    }

    // PrimaryExpression ::= Literal
    // Literal ::= <digit>+
    // <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
    fn primary_expression(&mut self) -> Option<Rc<Node>> {
        let t = match self.t.next() {
            Some(token) => token,
            None => return None,
        };

        match t {
            Token::Number(value) => Node::new_numeric_literal(value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    ExpressionStatement(Option<Rc<Node>>),
    AdditiveExpression {
        operator: char,
        left: Option<Rc<Node>>,
        right: Option<Rc<Node>>,
    },
    AssignmentExpression {
        operator: char,
        left: Option<Rc<Node>>,
        right: Option<Rc<Node>>,
    },
    MemberExpression {
        object: Option<Rc<Node>>,
        property: Option<Rc<Node>>,
    },
    NumericLiteral(u64),
}

impl Node {
    pub fn new_expression_statement(expression: Option<Rc<Self>>) -> Option<Rc<Self>> {
        Some(Rc::new(Node::ExpressionStatement(expression)))
    }

    pub fn new_additive_expression(
        operator: char,
        left: Option<Rc<Node>>,
        right: Option<Rc<Node>>,
    ) -> Option<Rc<Self>> {
        Some(Rc::new(Node::AdditiveExpression {
            operator,
            left,
            right,
        }))
    }

    pub fn new_assignment_expression(
        operator: char,
        left: Option<Rc<Node>>,
        right: Option<Rc<Node>>,
    ) -> Option<Rc<Self>> {
        Some(Rc::new(Node::AssignmentExpression {
            operator,
            left,
            right,
        }))
    }

    pub fn new_member_expression(
        object: Option<Rc<Self>>,
        property: Option<Rc<Self>>,
    ) -> Option<Rc<Self>> {
        Some(Rc::new(Node::MemberExpression { object, property }))
    }

    pub fn new_numeric_literal(value: u64) -> Option<Rc<Self>> {
        Some(Rc::new(Node::NumericLiteral(value)))
    }
}
