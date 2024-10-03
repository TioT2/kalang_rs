use super::*;

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, declaration) in &self.declarations {
            _ = f.write_fmt(format_args!("{name}: {declaration}\n\n"));
        }

        f.write_str(" ")
    }
}

impl std::fmt::Display for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const => f.write_str("const"),
            Self::Mut => f.write_str("mut"),
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::I8    => "i8",
            Self::U8    => "u8",
            Self::I16   => "i16",
            Self::U16   => "u16",
            Self::I32   => "i32",
            Self::U32   => "u32",
            Self::F32   => "f32",
            Self::I64   => "i64",
            Self::U64   => "u64",
            Self::F64   => "f64",
            Self::Usize => "usize",
            Self::Isize => "isize",
            Self::Char  => "char",
            Self::Void  => "void",
            Self::Never => "never",
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array { element_type, size } => {
                f.write_fmt(format_args!("[{}; {}]", element_type, size))
            }
            Self::FunctionPointer { inputs, output } => {
                f.write_str("fn(")?;
                if let Some((head, tail)) = inputs.split_first() {
                    head.fmt(f)?;
                    for arg in tail {
                        f.write_fmt(format_args!(", {arg}"))?;
                    }
                }
                f.write_fmt(format_args!(") {output}"))
            }
            Self::Named(name) => {
                f.write_str(name)
            }
            Self::Pointer { element_type, mutability } => {
                f.write_fmt(format_args!("*{mutability} {element_type}"))
            }
            Self::Primitive(primitive) => {
                f.write_fmt(format_args!("{primitive}"))
            }
            Self::Tuple { element_types } => {
                f.write_str("(")?;
                if let Some((head, tail)) = element_types.split_first() {
                    head.fmt(f)?;
                    for ty in tail {
                        f.write_fmt(format_args!(", {ty}"))?;
                    }
                }
                f.write_str(")")
            }
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("fn ("))?;

        if let Some((first, rest)) = self.inputs.split_first() {
            f.write_fmt(format_args!("{} {}: {}", first.1, first.0, first.2))?;
            for input in rest {
                f.write_fmt(format_args!(", {} {}: {}", input.1, input.0, input.2))?;
            }
        }

        f.write_fmt(format_args!(") {}", self.output))?;
        if self.implementation.is_some() {
            f.write_str(" { ... }")
        } else {
            f.write_str(";")
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "{} {}", self.mutability, self.ty);
        if let Some(initializer) = &self.initializer {
            r?;
            write!(f, " = {}", initializer)
        } else {
            r
        }
    }
}

impl std::fmt::Display for Structure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("struct {\n")?;
        for (name, ty) in &self.elements {
            f.write_fmt(format_args!("  {name}: {ty},\n"))?;
        }
        f.write_str("}")
    }
}

impl std::fmt::Display for Enumeration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {{\n")?;
        for (name, expr) in &self.variants {
            f.write_fmt(format_args!("    {}", name))?;
            if let Some(index) = &expr {
                f.write_fmt(format_args!(" = {}", index))?;
            }
            f.write_str(",\n")?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Enumeration(enu) => enu.fmt(f)?,
            Self::Function(func) => func.fmt(f)?,
            Self::Structure(str) => str.fmt(f)?,
            Self::Variable(var) => var.fmt(f)?,
        }

        f.write_str(" ")
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array { elements } => {
                f.write_str("[")?;
                if let Some((head, tail)) = elements.split_first() {
                    head.fmt(f)?;
                    for elem in tail {
                        f.write_str(", ")?;
                        elem.fmt(f)?;
                    }
                }
                f.write_str("]")
            }
            Self::BinaryOperator { lhs, rhs, operator } => {
                f.write_fmt(format_args!("({lhs} {operator} {rhs})"))
            }
            Self::Ident(ident) => {
                f.write_str(ident)
            }
            Self::IntegerConstant { number, postfix } => {
                number.fmt(f)?;
                postfix.fmt(f)
            }
            Self::FloatingConstant { number, postfix } => {
                number.fmt(f)?;
                postfix.fmt(f)
            }
            Self::StringConstant(str) => {
                str.fmt(f)
            }
            Self::CharacterConstant(char) => {
                char.fmt(f)
            }
            Self::Structure { type_name: name, fields } => {
                f.write_str(name)?;
                f.write_str("{ ")?;
                if let Some((head, tail)) = fields.split_first() {
                    f.write_fmt(format_args!("{}: {}", head.0, head.1))?;
                    for init in tail {
                        f.write_fmt(format_args!(", {}: {}", init.0, init.1))?;
                    }
                }
                f.write_str(" }")
            }
            Self::UnaryOperator { operand, operator } => {
                f.write_fmt(format_args!("{} {}", operand, operator))
            }
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Floating { number, postfix } => write!(f, "{}{}", number, postfix),
            Self::Integer { number, postfix } => write!(f, "{}{}", number, postfix),
            Self::String(str) => str.fmt(f),
            Self::Char(chr) => chr.fmt(f),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Add          => "+",
            Self::Sub          => "-",
            Self::Mul          => "*",
            Self::Div          => "/",
            Self::Shl          => "<<",
            Self::Shr          => ">>",
            Self::AddAssign    => "+=",
            Self::SubAssign    => "-=",
            Self::MulAssign    => "*=",
            Self::DivAssign    => "/=",
            Self::ShlAssign    => "<<=",
            Self::ShrAssign    => ">>=",
            Self::Lt           => "<",
            Self::Gt           => ">",
            Self::Le           => "<=",
            Self::Ge           => ">=",
            Self::Eq           => "==",
            Self::Ne           => "!=",
            Self::And          => "&&",
            Self::Or           => "||",
            Self::BitAnd       => "&",
            Self::BitOr        => "|",
            Self::BitXor       => "^",
            Self::AndAssign    => "&&=",
            Self::OrAssign     => "||=",
            Self::BitAndAssign => "&=",
            Self::BitOrAssign  => "|=",
            Self::BitXorAssign => "^=",
            Self::Assign       => "=",
        })
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("u")?;
        match self {
            Self::Call(args) => {
                f.write_str("(")?;
                if let Some((head, tail)) = args.split_first() {
                    head.fmt(f)?;
                    for arg in tail {
                        f.write_fmt(format_args!(", {arg}"))?;
                    }
                }
                f.write_str(")")
            }
            Self::Cast(ty) => {
                f.write_fmt(format_args!("as({ty})"))
            }
            Self::Dereference => {
                f.write_str("*")
            }
            Self::Index(indices) => {
                f.write_str("[")?;
                if let Some((head, tail)) = indices.split_first() {
                    head.fmt(f)?;
                    for arg in tail {
                        f.write_fmt(format_args!(", {arg}"))?;
                    }
                }
                f.write_str("]")
            }
            Self::Reference(mutability) => {
                f.write_fmt(format_args!("ref({mutability})"))
            }
            Self::UnaryMinus => {
                f.write_str("-")
            }
            Self::UnaryPlus => {
                f.write_str("+")
            }
            Self::FieldAccess(field) => {
                f.write_str(".")?;
                field.fmt(f)
            }
            Self::DereferencedFieldAccess(field) => {
                f.write_str("->")?;
                field.fmt(f)
            }
            Self::NamespaceAccess(element) => {
                f.write_str("::")?;
                element.fmt(f)
            }
        }
    }
}
