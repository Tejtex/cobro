mod ast;
mod evaluator;
mod setchecker;

use crate::evaluator::eval_program;
use crate::setchecker::setcheck_program;
use ast::*;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::io::Read;
use std::{io, thread};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct CobroParser;

fn parse_expr(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::comp | Rule::add_sub | Rule::mul_div | Rule::power => {
            let mut inner = pair.into_inner();
            let mut left = parse_expr(inner.next().unwrap());

            while let Some(op_pair) = inner.next() {
                let op = op_pair.as_str();
                let right = parse_expr(inner.next().unwrap());
                left = Expr::BinOp {
                    left: Box::new(left),
                    right: Box::new(right),
                    op: match op {
                        "+" => Operator::Add,
                        "*" => Operator::Mul,
                        "-" => Operator::Sub,
                        "/" => Operator::Div,
                        "%" => Operator::Mod,
                        "^" => Operator::Pow,
                        "=" => Operator::Equal,
                        _ => Operator::Add,
                    },
                };
            }

            left
        }

        Rule::primary | Rule::expr => parse_expr(pair.into_inner().next().unwrap()),
        Rule::number => Expr::Number(pair.as_str().parse().unwrap()),
        Rule::ident => Expr::Ident(pair.as_str().to_string()),
        Rule::func_call => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut args = Vec::new();
            for pair in inner {
                args.push(parse_expr(pair));
            }
            Expr::FuncCall { name, args }
        }
        Rule::if_else => {
            let mut inner = pair.into_inner();
            let cond = parse_expr(inner.next().unwrap());
            let then = parse_expr(inner.next().unwrap());
            let otherwise = parse_expr(inner.next().unwrap());
            Expr::IfElse {
                cond: Box::new(cond),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            }
        }
        _ => panic!(),
    }
}

fn parse_args(pair: Pair<Rule>) -> Vec<Arg> {
    pair.into_inner()
        .map(|inner| {
            let mut inner2 = inner.into_inner();
            let name = inner2.next().unwrap().as_str().to_string();
            let typ = parse_type(inner2.next().unwrap());
            Arg {
                name,
                typ: typ.expect("REASON"),
            }
        })
        .collect()
}

fn parse_stmt(pair: Pair<Rule>) -> Stmt {
    match pair.as_rule() {
        Rule::ret_stmt => {
            let expr = parse_expr(pair.into_inner().next().unwrap());
            Stmt::Ret(expr)
        }
        Rule::func_def => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let args = parse_args(inner.next().unwrap());
            let typ = parse_type(inner.next().unwrap());
            let body = parse_expr(inner.next().unwrap());
            Stmt::FuncDef(FuncDef {
                name,
                args,
                ret_type: typ.expect("REASON"),
                body,
            })
        }
        Rule::const_declaration => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let typ = parse_type(inner.next().unwrap());
            let expr = parse_expr(inner.next().unwrap());
            Stmt::Const {
                name,
                const_type: typ.expect("REASON"),
                value: expr,
            }
        }
        _ => panic!(),
    }
}

fn parse_type(pair: Pair<Rule>) -> anyhow::Result<Type> {
    match pair.as_str() {
        "Bool" => Ok(Type::Bool),
        "N" => Ok(Type::N),
        "Z" => Ok(Type::Z),
        "Q" => Ok(Type::Q),
        "R" => Ok(Type::R),

        _ => match pair.as_rule() {
            Rule::range_type => {
                let mut inner = pair.clone().into_inner();
                let mut inner_pairs = inner.clone().into_iter();

                let s = pair.as_str().trim();
                let inclusive_start = s.starts_with('[');
                let inclusive_end = s.ends_with(']');

                let start_pair = inner_pairs.next().unwrap();
                let end_pair = inner_pairs.next().unwrap();

                fn parse_bound(p: Pair<Rule>) -> Option<f64> {
                    if p.as_str().trim() == "_" {
                        None
                    } else {
                        Some(p.as_str().parse().unwrap())
                    }
                }

                Ok(Type::Range {
                    start: parse_bound(start_pair),
                    end: parse_bound(end_pair),
                    inclusive_start,
                    inclusive_end,
                })
            }

            Rule::typ => {
                let mut inner = pair.into_inner();
                parse_type(inner.next().ok_or(anyhow::Error::msg("type error"))?)
            }
            Rule::func_type => {
                let mut inner = pair.into_inner();
                let mut args = Vec::new();
                while let Some(pair) = inner.next() {
                    args.push(parse_type(pair)?);
                }

                Ok(Type::Func {
                    args: if args.len() as i32 - 1 > 0 {
                        args[0..args.len() - 1]
                            .iter()
                            .map(|el| el.clone())
                            .collect()
                    } else {
                        Vec::new()
                    },
                    ret: Box::from(args[args.len() - 1].clone()),
                })
            }
            _ => panic!(),
        },
    }
}

fn parse_program(mut pairs: Pairs<Rule>) -> Program {
    let mut stmts = Vec::new();
    let inner = pairs.next().unwrap().into_inner();
    for pair in inner {
        match pair.as_rule() {
            Rule::func_def | Rule::ret_stmt | Rule::const_declaration => {
                stmts.push(parse_stmt(pair))
            }
            _ => {}
        }
    }
    Program { stmts }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let pairs = CobroParser::parse(Rule::program, input.as_str()).expect("parse failed");

    let program = parse_program(pairs);
    match setcheck_program(program.clone()) {
        Ok(_) => (),
        Err(e) => {
            panic!("{}", e)
        }
    }
    let handler = thread::Builder::new()
        .stack_size(16 * 1024 * 1024) // 16 MB
        .spawn(move || {
            let eval = eval_program(&program);
            match eval {
                Ok(_) => (),
                Err(e) => panic!("{}", e),
            }
        })
        .unwrap();

    handler.join().unwrap();
}
