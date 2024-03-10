* async if you are brave enough :)
* implement string escaping "123\"456" => "123"456"
* return is not implemented yet (want to keep it in fn only)
* implement array slicing (tail built-in fn)

page 180

Evaluating Expressions

## Interpreter supports functions, higher-order functions, closures, strings, integers and arithmetic.

Example of usage
```
// method and var definition
let add = fn(a, b) { a + b };
let applyFunc = fn(a, b, func) { func(a, b) };

let res = applyFunc(2, 2, add);
res
=> 4

// method call
puts(res);
=> 4
=> null

// strings
"this is a string";

// strings can be mutable
let arr = ["string is too long"]
let first = arr[0]

first
=> "string is too long"

truncate(first, 3)
first
=> "str"

arr
=> ["str"]


// built-in functions
len("Hello" + " " + "World!")
=> 12
len([1, 2, false, "string"])
=> 4

// arrays
let a = [1, 2, false, "string"];
a[0]
=> 1

a[3]
=> "string"

a[4]
=> index out of range: 4

```
