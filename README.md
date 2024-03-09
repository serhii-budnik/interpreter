* implement string escaping "123\"456" => "123"456"
* return is not implemented yet (want to keep it in fn only)

page 164

Evaluating Expressions

## Interpreter supports functions, higher-order functions, closures, strings, integers and arithmetic.

Example of usage
```
let add = fn(a, b) { a + b };
let applyFunc = fn(a, b, func) { func(a, b) };

let res = applyFunc(2, 2, add);
res
=> 4

puts(res);
=> 4
=> null

"this is a string";

len("Hello" + " " + "World!")
=> 12

```
