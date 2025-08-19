use std::collections::HashMap;
use anyhow::{anyhow, bail};
use crate::ast::{Arg, Expr, FuncDef, Operator, Program, Stmt, Type};

#[derive(Clone, Debug)]
enum Object {
    Number(f64),
    Bool(bool),
    Func(Function)
}

#[derive(Clone, Debug)]
struct Function {
    body: Expr,
    args: Vec<Arg>
}

#[derive(Clone, Default, Debug)]
struct EvalEnv<'a> {
    consts: HashMap<String, Object>,
    parent: Option<&'a EvalEnv<'a>>,
}

impl<'a> EvalEnv<'a> {
    fn get(&self, name: &str) -> Option<Object> {
        self.consts.get(name)
            .cloned()
            .or_else(|| self.parent.and_then(|p| p.get(name)))
    }
}


pub fn eval_program(program: &Program) -> anyhow::Result<()> {
    let mut env = EvalEnv::default();

    for stmt in program.stmts.iter() {
        eval_stmt(stmt, &mut env)?;
    }
    Ok(())
}

fn eval_stmt(stmt: &Stmt, env: &mut EvalEnv) -> anyhow::Result<()> {
    match stmt {
        Stmt::Const {name, value, const_type} => {
            let expr = eval_expr(value, env)?;
            env.consts.insert(name.clone(), expr);
            Ok(())
        }
        Stmt::Ret(expr) => {
            let expr = eval_expr(expr, env)?;
            match expr {
                Object::Number(number) => println!("{}", number),
                Object::Func (Function{args,  body :expr }) => println!("printing function doesn't work LOL"),
                Object::Bool(b) => println!("{}", b),
            }
            Ok(())
        }
        Stmt::FuncDef(FuncDef { name, args, ret_type, body}) => {
            env.consts.insert(name.clone(), Object::Func(Function {body: body.clone(), args: args.clone()}));
            Ok(())
        }
    }
}

fn eval_expr(expr: &Expr, env: &mut EvalEnv) -> anyhow::Result<Object> {
    match expr {
        Expr::Number(n) => Ok(Object::Number(*n)),
        Expr::Ident(name) => {
            if let Some(a) = env.get(name) {
                Ok(a.clone())
            } else {
                bail!("no function or constant named {}", name)
            }
        }
        ,
        Expr::FuncCall {name, args} => {
            let func = match env.get(name).clone().ok_or_else(|| anyhow!("no function named {}", name))? {
                Object::Func(func) => func,
                _ => bail!("unknown function {}", name)
            };

            let mut local_env = EvalEnv::default();
            for (arg, param) in args.iter().zip(&func.args) {
                local_env.consts.insert(param.name.clone(), eval_expr(arg, env)?);
            }
            local_env.parent = Some(env);
            let result = eval_expr(&func.body, &mut local_env)?;
            Ok(result)


        }
        Expr::BinOp { op, left, right } => {
            let left = eval_expr(left, env)?;
            let right = eval_expr(right, env)?;

            Ok(eval_bin_op(left, right, op.clone())?)
        }
        Expr::IfElse {cond,  then, otherwise} => {
            let cond = eval_expr(cond, env)?;
            let cond = match cond {
                Object::Bool(true) => true,
                Object::Bool(false) => false,
                Object::Number(n) => n != 0.0,
                Object::Func(_) => false
            };

            if cond {

                Ok(eval_expr(then, env)?)
            } else {

                Ok(eval_expr(otherwise, env)?)
            }
        }

    }
}

fn eval_bin_op(left: Object, right: Object, op: Operator) -> anyhow::Result<Object> {
    if let Object::Number(a) = left {
        if let Object::Number(b) = right {
            return Ok(match op {
                Operator::Add =>Object::Number(a + b),
                Operator::Sub => Object::Number(a - b),
                Operator::Mul => Object::Number(a * b),
                Operator::Div => Object::Number(a / b),
                Operator::Mod => Object::Number(a % b),
                Operator::Pow => Object::Number(a.powf(b)),
                Operator::Equal => Object::Bool(a == b),
                _ => {Object::Number(0.)}
            })
        }
    }

    bail!("cannot apply math operations on functions!")

}