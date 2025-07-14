use paste::paste;

use crate::token::Token;

pub trait Walkable<V, T> {
    fn walk(&self, visitor: V) -> T;
}

macro_rules! ast_impl {
    ($name:ident, struct, {$($vis:vis $field:ident: $field_type:ty),* $(,)?}) => {
        impl $name {
            pub fn new($($field: $field_type),*) -> Self {
                Self {
                    $($field),*
                }
            }
        }
    };
    ($name:ident, enum, $block:tt) => {};
    ($name:ident, struct, s, {$($vis:vis $field:ident: $field_type:ty),* $(,)?}) => {
        paste! {
            pub fn [<new_ $name:lower>]($($field: $field_type),*) -> Self {
                Self::$name($name::new($($field),*))
            }
        }
    };
    ($name:ident, enum, s, $block:tt) => {};
}

macro_rules! ast {
    ($($base_name:ident {$($name:ident: $typ:ident $block:tt),* $(,)?}),* $(,)?) => {
        $(
            #[derive(Debug, Clone)]
            pub enum $base_name {
                $($name($name)),*
            }

            impl $base_name {
                $(ast_impl!($name, $typ, s, $block);)*
            }

            $(
                #[derive(Debug, Clone)]
                pub $typ $name $block

                ast_impl!($name, $typ, $block);
            )*

            paste! {
                pub trait [<$base_name Visitor>]<R> {
                    $(fn [<visit_ $name:lower>](self, [<$base_name:lower>]: &$name) -> R;)*
                }
            }

            paste! {
                impl<V: [<$base_name Visitor>]<T>, T> Walkable<V, T> for $base_name {
                    fn walk(&self, visitor: V) -> T {
                        match self {
                            $(
                                $base_name::$name(x) => visitor.[<visit_ $name:lower>](x)
                            ),*
                        }
                    }
                }
            }
        )*
    };
}

ast! {
    Expr {
        Binary: struct {
            pub left: Box<Expr>,
            pub operator: Token,
            pub right: Box<Expr>,
        },
        Grouping: struct {
            pub expr: Box<Expr>,
        },
        Literal: enum {
            String(String),
            Number(f64),
            True,
            False,
            Nil,
        },
        Unary: struct {
            pub operator: Token,
            pub right: Box<Expr>,
        },
        Variable: struct {
            pub name: Token,
        },
    },
    Stmt {
        Expression: struct {
            pub expr: Expr,
        },
        Print: struct {
            pub expr: Expr,
        },
        Var: struct {
            pub name: Token,
            pub initializer: Option<Expr>,
        },
    },
}
