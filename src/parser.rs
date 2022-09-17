#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    Integer(u32),
    Ident(String),
    ETimes(Box<Exp>, Box<Exp>),
    EPlus(Box<Exp>, Box<Exp>),
}

pub(crate) fn etimes(l: Exp, r: Exp) -> Exp {
    Exp::ETimes(Box::new(l), Box::new(r))
}

pub(crate) fn eplus(l: Exp, r: Exp) -> Exp {
    Exp::EPlus(Box::new(l), Box::new(r))
}

pub(crate) fn integer(int: u32) -> Exp {
    Exp::Integer(int)
}

pub(crate) fn ident(id: &str) -> Exp {
    Exp::Ident(id.to_owned())
}

fn parse(input: &str) -> Exp {
    peg::parser! {
        grammar simp_parser() for str {
            rule integer() -> Exp
                = n:$(['0'..='9']+) { Exp::Integer(n.parse().unwrap()) }

            rule ident() -> Exp
                = x:$(['a'..='z']) { Exp::Ident(x.to_owned()) }

            rule exp2() -> Exp
                = integer() / ident()

            #[cache_left_rec]
            rule exp1() -> Exp
                = l:exp1() " * " r:exp2() {
                    Exp::ETimes(Box::new(l), Box::new(r))
                } / exp2()

            #[cache_left_rec]
            pub rule exp() -> Exp
                = l:exp() " + " r:exp1() {
                    Exp::EPlus(Box::new(l), Box::new(r))
                } / exp1()
        }
    }

    simp_parser::exp(input).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant() {
        let tree = parse(include_str!("../inputs/const.simp"));
        assert_eq!(tree, integer(5));
    }

    #[test]
    fn add() {
        let tree = parse(include_str!("../inputs/add.simp"));
        assert_eq!(tree, eplus(integer(5), integer(4)));
    }

    #[test]
    fn mul() {
        let tree = parse(include_str!("../inputs/mul.simp"));
        assert_eq!(tree, etimes(integer(4), integer(5)));
    }

    #[test]
    fn mixed_ops() {
        let tree = parse(include_str!("../inputs/mixed-ops.simp"));
        assert_eq!(
            tree,
            eplus(
                etimes(etimes(integer(5), integer(2)), integer(4)),
                integer(3)
            )
        );
    }

    #[test]
    fn parens() {
        let tree = parse(include_str!("../inputs/parens.simp"));
        assert_eq!(tree, integer(5));
    }

    #[test]
    fn open_mixed() {
        let tree = parse(include_str!("../inputs/open-mixed.simp"));
        assert_eq!(tree, eplus(etimes(integer(5), ident("x")), ident("y")));
    }
}
