#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident(String);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    EInt(u32),
    EVar(Ident),
    ETimes(Box<Exp>, Box<Exp>),
    EPlus(Box<Exp>, Box<Exp>),
}

pub(crate) fn etimes(l: Exp, r: Exp) -> Exp {
    Exp::ETimes(Box::new(l), Box::new(r))
}

pub(crate) fn eplus(l: Exp, r: Exp) -> Exp {
    Exp::EPlus(Box::new(l), Box::new(r))
}

pub(crate) fn eint(int: u32) -> Exp {
    Exp::EInt(int)
}

pub(crate) fn evar(id: &str) -> Exp {
    Exp::EVar(Ident(id.to_owned()))
}

fn parse(input: &str) -> Exp {
    peg::parser! {
        grammar simp_parser() for str {
            rule integer() -> Exp
                = n:$(['0'..='9']+) { eint(n.parse().unwrap()) }

            rule ident() -> Exp
                = x:$(['a'..='z']) { evar(x) }

            rule exp2() -> Exp
                = integer() / ident()

            #[cache_left_rec]
            rule exp1() -> Exp
                = l:exp1() " * " r:exp2() {
                    etimes(l, r) 
                } / exp2()

            #[cache_left_rec]
            pub rule exp() -> Exp
                = l:exp() " + " r:exp1() {
                    eplus(l, r)
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
        assert_eq!(tree, eint(5));
    }

    #[test]
    fn add() {
        let tree = parse(include_str!("../inputs/add.simp"));
        assert_eq!(tree, eplus(eint(5), eint(4)));
    }

    #[test]
    fn mul() {
        let tree = parse(include_str!("../inputs/mul.simp"));
        assert_eq!(tree, etimes(eint(4), eint(5)));
    }

    #[test]
    fn mixed_ops() {
        let tree = parse(include_str!("../inputs/mixed-ops.simp"));
        assert_eq!(
            tree,
            eplus(
                etimes(etimes(eint(5), eint(2)), eint(4)),
                eint(3)
            )
        );
    }

    #[test]
    fn parens() {
        let tree = parse(include_str!("../inputs/parens.simp"));
        assert_eq!(tree, eint(5));
    }

    #[test]
    fn open_mixed() {
        let tree = parse(include_str!("../inputs/open-mixed.simp"));
        assert_eq!(tree, eplus(etimes(eint(5), evar("x")), evar("y")));
    }
}
