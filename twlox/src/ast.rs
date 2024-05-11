/// Generates boilerplate code for AST. It includes: Visitor trait, accept
/// method, enum, and a separate structure for each enum variant.
///
/// For a better understanding, see where this macro is already used. All
/// meta information (e.g. derive) that the enum has will be propagated to all
/// structs/enums.
macro_rules! ast {
    // Matches full enum with meta information.
    (
        #[$meta:meta]
        enum $enum_name:ident {
            $($name:ident {
                $($body:tt)*
            },)*
        }
    ) => {paste::paste! {
        // Create enum with replaced variants values.
        #[$meta]
        pub enum $enum_name {
            $($name($name),)*
        }

        // Create trait Visitor for the enum that contains a function to visit each variant.
        pub trait [<$enum_name Visitor>]<T> {
            $(fn [<visit_$name:lower>](&mut self, [<$enum_name:lower>]: &$name) -> T;)*
        }

        // Create accept (visitor pattern) method for the enum that matches each variant.
        impl $enum_name {
            pub fn accept<T, V: [<$enum_name Visitor>]<T>>(&self, visitor: &mut V) -> T {
                match self {
                    $($enum_name::$name(e) => visitor.[<visit_ $name:lower>](e),)*
                }
            }
        }

        // Create separate struct/enum for each enum variant.
        $(ast_helper! {
            $enum_name
            #[$meta]
            $name {
                $($body)*
            }
        })*
    }};
}

/// Responsible for creating an enum or struct and its constructor method.
macro_rules! ast_helper {
    (
        $enum_name:ident
        #[$meta:meta]
        $name:ident {
            $($field:ident$(($type:ty))?,)*
        }
    ) => { paste::paste! {
        #[$meta]
        pub enum $name {
            $($field$(($type))?),*
        }
    }};

    (
        $enum_name:ident
        #[$meta:meta]
        $name:ident {
            $($field:ident : $field_type:ty,)*
        }
    ) => { paste::paste! {
        #[$meta]
        pub struct $name {
            $(pub $field: $field_type),*
        }

        impl $enum_name {
            // Create `new_<name>` constructor method.
            pub fn [<new_ $name:lower>]($($field: $field_type,)*) -> Self {
                Self::$name($name {
                    $($field,)*
                })
            }
            // Create `is_<name>` method to verify specific enum variant.
            pub fn [<is_ $name:lower>](&self) -> bool {
                matches!(self, Self::$name(_))
            }
        }
    }};
}

pub mod stmt {
    use super::expr::Expr;
    use crate::scanner::token::Token;

    ast! {
        #[derive(Debug, Clone, PartialEq)]
        enum Stmt {
            Block {
                statements: Vec<Stmt>,
            },
            Class {
                name: Token,
                methods: Vec<Function>,
            },
            Expression {
                expression: Expr,
            },
            Function {
                name: Token,
                params: Vec<Token>,
                body: Vec<Stmt>,
            },
            If {
                condition: Expr,
                then_branch: Box<Stmt>,
                else_branch: Option<Box<Stmt>>,
            },
            Return {
                keyword: Token,
                value: Option<Expr>,
            },
            Var {
                name: Token,
                initializer: Option<Expr>,
            },
            While {
                condition: Expr,
                statement: Box<Stmt>,
            },
        }
    }

    impl Stmt {
        pub fn to_box(self) -> Box<Self> {
            self.into()
        }
    }
}

pub mod expr {
    use std::fmt::Display;

    use crate::scanner::token::Token;

    ast! {
        #[derive(Debug, Clone, PartialEq)]
        enum Expr {
            Literal {
                Number(f64),
                String(String),
                Bool(bool),
                Nil,
            },
            Call {
                callee: Box<Expr>,
                arguments: Vec<Expr>,
                closing_paren: Token,
            },
            Get {
                obj: Box<Expr>,
                name: Token,
            },
            Set {
                obj: Box<Expr>,
                name: Token,
                value: Box<Expr>,
            },
            Unary {
                op: Token,
                inner: Box<Expr>,
            },
            Logical {
                left: Box<Expr>,
                right: Box<Expr>,
                op: Token,
            },
            Grouping {
                inner: Box<Expr>,
            },
            Binary {
                left: Box<Expr>,
                right: Box<Expr>,
                op: Token,
            },
            Assign {
                name: Token,
                value: Box<Expr>,
            },
            Variable {
                name: Token,
            },
            This {
                keyword: Token,
            },
        }
    }

    impl Display for Expr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expr::Literal(l) => match l {
                    Literal::Number(n) => n.fmt(f),
                    Literal::String(s) => s.fmt(f),
                    Literal::Bool(b) => b.fmt(f),
                    Literal::Nil => "nil".fmt(f),
                },
                Expr::Call(c) => {
                    write!(f, "{} (", c.callee)?;
                    let mut iter = c.arguments.iter().peekable();
                    while let Some(arg) = iter.next() {
                        write!(f, "{}", arg)?;
                        if iter.peek().is_some() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                }
                Expr::Unary(u) => write!(f, "{}{}", u.op.lexeme(), u.inner),
                Expr::Logical(l) => {
                    write!(f, "{} {} {}", l.left, l.op.lexeme(), l.right)
                }
                Expr::Grouping(g) => write!(f, "({})", g.inner),
                Expr::Binary(b) => {
                    write!(f, "{} {} {}", b.left, b.op.lexeme(), b.right)
                }
                Expr::Assign(a) => {
                    write!(f, "{} = {}", a.name.lexeme(), a.value)
                }
                Expr::Variable(v) => v.name.lexeme().fmt(f),
                Expr::Get(g) => write!(f, "{}.{}", g.obj, g.name.lexeme()),
                Expr::Set(s) => {
                    write!(f, "{}.{} = {}", s.obj, s.name.lexeme(), s.value)
                }
                Expr::This(t) => write!(f, "{}", t.keyword.lexeme()),
            }
        }
    }

    // Methods like "new_<enum_option>" are generated in ast_helper! macro.
    impl Expr {
        pub fn to_box(self) -> Box<Self> {
            self.into()
        }

        pub fn new_number(value: f64) -> Self {
            Self::Literal(Literal::Number(value))
        }

        pub fn new_string(value: String) -> Self {
            Self::Literal(Literal::String(value))
        }

        pub fn new_bool(value: bool) -> Self {
            Self::Literal(Literal::Bool(value))
        }

        pub fn new_nil() -> Self {
            Self::Literal(Literal::Nil)
        }
    }
}
