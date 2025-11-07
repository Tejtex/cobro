use crate::ast::{Expr, FuncDef, Program, Stmt, Type};
use crate::parse_expr;
use anyhow::{Result, bail};
use num_rational::Ratio;
use std::cmp::Ordering::Equal;
use std::cmp::{Ordering, PartialEq};
use std::collections::{HashMap, HashSet};
/// Unions two sets.
///
/// Example:
/// union([1, 2], [4, 5)) -> [1, 5)
/// union(N, Q) -> Q
fn union(a: &Type, b: &Type) -> Result<Type> {
    match (a, b) {
        (Type::Func { args, ret }, _) => bail!("cannot apply math operators on functions"),
        (_, Type::Func { args, ret }) => bail!("cannot apply math operators on functions"),
        (
            Type::Range {
                start: None,
                end: None,
                ..
            },
            _,
        ) => Ok(Type::R),
        (
            _,
            Type::Range {
                start: None,
                end: None,
                ..
            },
        ) => Ok(Type::R),
        (_, Type::R) => Ok(Type::R),
        (Type::R, _) => Ok(Type::R),
        (_, Type::Q) => Ok(Type::Q),
        (Type::Q, _) => Ok(Type::Q),
        (_, Type::Z) => Ok(Type::Z),
        (Type::Z, _) => Ok(Type::Z),
        (_, Type::N) => Ok(Type::N),
        (Type::N, _) => Ok(Type::N),
        (Type::Set(a_set), Type::Set(b_set)) => {
            let mut result = a_set.clone();
            result.extend(b_set.iter().cloned());
            result.sort_by(|x, y| x.partial_cmp(y).unwrap());
            result.dedup();
            Ok(Type::Set(result))
        }

        (
            Type::Set(a),
            Type::Range {
                start,
                end,
                inclusive_start: is,
                inclusive_end: ie,
            },
        )
        | (
            Type::Range {
                start,
                end,
                inclusive_start: is,
                inclusive_end: ie,
            },
            Type::Set(a),
        ) => {
            let min = a.iter().cloned().reduce(f64::min).unwrap();
            let max = a.iter().cloned().reduce(f64::max).unwrap();
            let (new_start, new_is) = match (start, min) {
                (None, _) => (None, false),
                (Some(a), b) => {
                    if *a < b {
                        (Some(*a), *is)
                    } else {
                        (Some(b), true)
                    }
                }
            };
            let (new_end, new_ie) = match (end, max) {
                (None, _) => (None, false),
                (Some(a), b) => {
                    if *a > b {
                        (Some(*a), *ie)
                    } else {
                        (Some(b), true)
                    }
                }
            };
            Ok(Type::Range {
                start: new_start,
                end: new_end,
                inclusive_end: new_ie,
                inclusive_start: new_is,
            })
        }
        // Unreadable just combines two ranges
        (
            Type::Range {
                start,
                end,
                inclusive_start: is,
                inclusive_end: ie,
            },
            Type::Range {
                start: start2,
                end: end2,
                inclusive_end: ie2,
                inclusive_start: is2,
            },
        ) => {
            let new_start = match (start, start2) {
                (_, None) => None,
                (None, _) => None,
                (Some(a), Some(b)) => Some((*a).min(*b)),
            };
            let new_is = if new_start == None {
                false
            } else {
                (if new_start == *start { *is } else { false })
                    || (if new_start == *start2 { *is2 } else { false })
            };

            let new_end = match (end, end2) {
                (_, None) => None,
                (None, _) => None,
                (Some(a), Some(b)) => Some((*a).max(*b)),
            };
            let new_ie = if new_end == None {
                false
            } else {
                (if new_end == *end { *ie } else { false })
                    || (if new_end == *end2 { *ie2 } else { false })
            };
            Ok(Type::Range {
                start: new_start,
                end: new_end,
                inclusive_start: new_is,
                inclusive_end: new_ie,
            })
        }
        (_, Type::Bool) => Ok(Type::Bool),
        (Type::Bool, _) => Ok(Type::Bool),
    }
}

fn set_contains(a: &Type, b: &Type) -> Result<bool> {
    match (a, b) {
        (Type::N, Type::N) => Ok(true),
        (Type::N, Type::Q) => Ok(true),
        (Type::N, Type::Z) => Ok(true),
        (Type::N, Type::R) => Ok(true),
        (Type::Z, Type::Z) => Ok(true),
        (Type::Z, Type::Q) => Ok(true),
        (Type::Z, Type::R) => Ok(true),
        (Type::Q, Type::Q) => Ok(true),
        (Type::Q, Type::R) => Ok(true),
        (Type::R, Type::R) => Ok(true),
        (
            Type::Range {
                start: _,
                end: _,
                inclusive_start: _,
                inclusive_end: _,
            },
            Type::R,
        ) => Ok(true),

        (Type::Set(_), Type::R) => Ok(true),

        (Type::Set(s), Type::N) => Ok(s.iter().all(|el| el.trunc() == *el && *el >= 0.)),

        (Type::Set(s), Type::Z) => Ok(s.iter().all(|el| el.trunc() == *el)),

        (Type::Set(s), Type::Q) => {
            let epsilon = 1e-10;
            Ok(s.iter()
                .map(|el| {
                    let rat: Ratio<i32> = Ratio::approximate_float(*el)
                        .ok_or_else(|| anyhow::anyhow!("Not a rational"))?;

                    Ok::<(Ratio<i32>, f64), anyhow::Error>((rat, *el))
                })
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .all(|(rat, el)| {
                    ((*rat.numer() as f64) / (*rat.denom() as f64) - *el).abs() < epsilon
                }))
        }

        // // Is a set in another set? NOTE: NOT ALL NUMBERS FROM SET B HAVE TO BE IN SET A
        (Type::Set(a), Type::Set(b)) => Ok(a.iter().all(|el| {
            b.binary_search_by(|el2| el.partial_cmp(el2).unwrap_or(Equal))
                .is_ok()
        })),

        // Is a set of numbers in a range?
        (
            Type::Set(s),
            Type::Range {
                start: a,
                end: b,
                inclusive_start: ia,
                inclusive_end: ib,
            },
        ) => Ok(s.iter().all(|el| match (a, b) {
            (Some(a), Some(b)) => {
                let left = if *ia { el >= a } else { el > a };
                let right = if *ib { el <= b } else { el < b };
                left && right
            }
            (None, Some(b)) => {
                if *ib {
                    el <= b
                } else {
                    el < b
                }
            }
            (Some(a), None) => {
                if *ia {
                    el >= a
                } else {
                    el > a
                }
            }

            (None, None) => true,
        })),

        // Is a range in a range?
        (
            Type::Range {
                start: a1,
                end: b1,
                inclusive_start: ia1,
                inclusive_end: ib1,
            },
            Type::Range {
                start: a2,
                end: b2,
                inclusive_start: ia2,
                inclusive_end: ib2,
            },
        ) => {
            let left = match (a1, a2) {
                (Some(a1), Some(a2)) => {
                    if ia1 == ia2 {
                        a1 >= a2
                    } else {
                        a1 > a2
                    }
                }
                (Some(_), None) => false,
                _ => true,
            };
            let right = match (b1, b2) {
                (Some(b1), Some(b2)) => {
                    if ib1 == ib2 {
                        b1 <= b2
                    } else {
                        b1 < b2
                    }
                }
                (None, Some(_)) => false,
                _ => true,
            };
            Ok(left && right)
        }
        _ => Ok(false),
    }
}

struct FuncType {
    args: Vec<Type>,
    ret_type: Type,
}

#[derive(Default)]
struct SetCheckEnv {
    pub consts: HashMap<String, Type>,
    pub functions: HashMap<String, FuncType>,
}

fn setcheck_expr(expr: Expr, env: &SetCheckEnv) -> anyhow::Result<Type> {
    match expr {
        Expr::Number(x) => Ok(Type::Set(vec![x])),
        Expr::Ident(name) => {
            if env.consts.contains_key(&name) {
                Ok(env.consts[&name].clone())
            } else {
                bail!("constant {} not found", name)
            }
        }
        Expr::BinOp { left, right, op } => {
            let left = setcheck_expr(*left, &env)?;
            let right = setcheck_expr(*right, &env)?;

            if set_contains(&left, &right)? {
                Ok(right)
            } else if set_contains(&right, &left)? {
                Ok(left)
            } else {
                Ok(union(&left, &right)?)
            }
        }
        Expr::FuncCall { name, args } => {
            if env.functions.contains_key(&name) {
                Ok(env.functions[&name].ret_type.clone())
            } else {
                bail!("function {} not found", name)
            }
        }
        Expr::IfElse {
            cond,
            then,
            otherwise,
        } => {
            let then = setcheck_expr(*then, env)?;
            let otherwise = setcheck_expr(*otherwise, env)?;
            if then == otherwise {
                Ok(then)
            } else {
                bail!("the types in if else should be the same in both branches")
            }
        }
    }
}

fn setcheck_stmt(stmt: Stmt, env: &mut SetCheckEnv) -> Result<()> {
    match stmt {
        Stmt::Const {
            name,
            const_type,
            value,
        } => {
            let expr = setcheck_expr(value, env)?;
            if !set_contains(&expr, &const_type)? {
                bail!(
                    "trying to assign expr of type {:?} to a const with type {:?}",
                    expr,
                    const_type
                )
            }

            env.consts.insert(name, const_type.clone());

            Ok(())
        }
        Stmt::FuncDef(FuncDef {
            name,
            args,
            ret_type,
            body,
        }) => {
            env.functions.insert(
                name,
                FuncType {
                    args: args.iter().map(|arg| arg.typ.clone()).collect(),
                    ret_type: ret_type.clone(),
                },
            );
            for arg in &args {
                match &arg.typ {
                    Type::Func { args, ret } => {
                        env.functions.insert(
                            arg.name.clone(),
                            FuncType {
                                args: args.clone(),
                                ret_type: *ret.clone(),
                            },
                        );
                    }
                    _ => {
                        env.consts.insert(arg.name.clone(), arg.typ.clone());
                    }
                };
            }

            let body = setcheck_expr(body, env)?;
            if !set_contains(&body, &ret_type)? {
                bail!(
                    "function is returning type {:?} but declared type is {:?}",
                    body,
                    ret_type
                )
            }

            for arg in &args {
                match &arg.typ {
                    Type::Func { args, ret } => {
                        env.functions.remove(&arg.name.clone());
                    }
                    _ => {
                        env.consts.remove(&arg.name.clone());
                    }
                };
            }
            Ok(())
        }
        Stmt::Ret(expr) => {
            setcheck_expr(expr, &env)?;
            Ok(())
        }
    }
}

pub fn setcheck_program(program: Program) -> Result<()> {
    let mut env = SetCheckEnv::default();
    for stmt in program.stmts {
        setcheck_stmt(stmt, &mut env)?;
    }
    Ok(())
}
