use crate::object::{ObjectType, NULL_OBJ};
use std::collections::VecDeque;
use ::phf::{Map, phf_map};

fn puts(args: VecDeque<ObjectType>) -> ObjectType {
    let first_arg = match args.get(0) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 1".to_string()),
    };

    println!("{}", first_arg);

    NULL_OBJ
}

pub static BUILTIN_FNS: Map<&'static str, fn(VecDeque<ObjectType>) -> ObjectType> = phf_map! {
    "puts" => puts,
};
