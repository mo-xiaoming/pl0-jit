# static/jit compiler for PL0

```
program = block "." .

block =
    ["const" ident "=" num {"," ident "=" num} ";"]
    ["var" ident {"," ident} ";"]
    {"procedure" ident ";" block ";"} statement .

statement =
    ident ":=" expression
    | "call" ident
    | "?" ident
    | "!" expression
    | "begin" statement {";" statement } "end"
    | "if" condition "then" statement
    | "while" condition "do" statement .

condition =
    "odd" expression
    | expression ("="|"#"|"<"|"<="|">"|">=") expression .

expression = ["+"|"-"] term {("+"|"-") term} .

term = factor {("*"|"/") factor} .

factor =
    ident
    | number
    | "(" expression ")" .
```

## Limitations

Too lazy to fix them / Not a big fan of Pascal

1. `const` doesn't accept negative number
2. No line numbers on error messages, just something like,

    ```
    parse_error_unexpected_t: expected number, but got -
         00|const a = -3;.
                      ^
    ```
3. Can do better with function redefinitions error, just like variable redefinitions

    ```
    parse_error_name_redefined_t: x previously defined at
         00|  const x = 42;
                    ^
    redefined at
         00|  const x = 7;
                    ^
    ```
4. Errors can be more ergonomics in general
5. Reading from source file is broken, file buffer is released after lexing, which renders every token invalid
6. Didn't do enough error checking on codegen phase
