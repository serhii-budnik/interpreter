use crate::object::{ObjectType, NULL_OBJ};
use ::phf::{Map, phf_map};

fn puts(args: Vec<ObjectType>) -> ObjectType {
    let first_arg = match args.get(0) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 1".to_string()),
    };

    println!("{}", first_arg);

    NULL_OBJ
}

fn len(args: Vec<ObjectType>) -> ObjectType {
    let first_arg = match args.get(0) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 1".to_string()),
    };

    match first_arg {
        ObjectType::OString(s) => ObjectType::Int(s.borrow().len() as isize),
        ObjectType::Array(arr) => ObjectType::Int(arr.len() as isize),
        _ => ObjectType::new_error(format!("argument to `len` not supported, got {}", first_arg.type_name())),
    }
}

pub static BUILTIN_FNS: Map<&'static str, fn(Vec<ObjectType>) -> ObjectType> = phf_map! {
    "puts" => puts,
    "len" => len,
};
