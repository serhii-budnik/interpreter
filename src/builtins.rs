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
        ObjectType::OString(s) => ObjectType::Int(s.as_ref().borrow().len() as isize),
        ObjectType::Array(arr) => ObjectType::Int(arr.len() as isize),
        _ => ObjectType::new_error(format!("argument to `len` not supported, got {}", first_arg.type_name())),
    }
}

// returns the last element of an array or null if the array is empty
fn last(args: Vec<ObjectType>) -> ObjectType {
    let first_arg = match args.get(0) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 1".to_string()),
    };

    match first_arg {
        ObjectType::Array(arr) => {
            let arr_len = arr.len();

            if arr_len == 0 { return NULL_OBJ }

            arr.get(arr_len - 1).unwrap().clone()
        },
        _ => ObjectType::new_error(format!("argument to `len` not supported, got {}", first_arg.type_name())),
    }
}

/// Mutates the existing string
///
/// Shortens this String to the specified length.
///
/// If new_len is greater than the string’s current length, this has no effect.
fn truncate(args: Vec<ObjectType>) -> ObjectType {
    let first_arg = match args.get(0) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 2".to_string()),
    };

    let second_arg = match args.get(1) {
        Some(arg) => arg,
        None => return ObjectType::new_error("got no arguments, expected 2".to_string()),
    };

    match (first_arg, second_arg) {
        (ObjectType::OString(first), ObjectType::Int(num)) => {
            first.as_ref().borrow_mut().truncate(*num as usize);

            NULL_OBJ

        },
        _ => ObjectType::new_error(format!("argument to `truncate` not supported, got {}", first_arg.type_name())),
    }
}

pub static BUILTIN_FNS: Map<&'static str, fn(Vec<ObjectType>) -> ObjectType> = phf_map! {
    "puts" => puts,
    "len" => len,
    "last" => last,
    "truncate" => truncate,
};
