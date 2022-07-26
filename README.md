# jit compiler for [PL0](https://en.wikipedia.org/wiki/PL/0)

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

## Build and run

This is a typical cmake project, just guarantee `gtest` and `llvm` can be found by cmake.

The default build type is debug build with address and undefined sanitizers.

Executing the built binary `pl0-jit`, you'll see `42` gets printed.

The source code is embedded in [src/main.cpp](src/main.cpp)

```pascal
procedure print_42_out_of_100;
var a;
begin
  a := 100;
  while a > 10 do
  begin
    a := a-1;
    if a = 13 then
      !2 + 5 * (a - 5)
  end
end;
call print_42_out_of_100
.
```

## Limitations

Too lazy to fix them / Not a big fan of Pascal

1. Doesn't handle accessing variables in parent scope well, because I totally forget the Pascal grammar, and too lazy to check how PL0 works
2. Tried to refactor codegen part to llvm c api, then found out they are not friendly with non-null-terminated strings
3. `const` doesn't accept negative number
4. No line numbers on error messages, just something like,

    ```
    parse_error_unexpected_t: expected number, but got -
         00|const a = -3;.
                      ^
    ```
5. Can do better with function redefinitions error, just like variable redefinitions

    ```
    parse_error_name_redefined_t: x previously defined at
         00|  const x = 42;
                    ^
    redefined at
         00|  const x = 7;
                    ^
    ```
6. Errors can be more ergonomics in general
7. Reading from source file is broken, file buffer is released after lexing, which renders every token invalid
8. Didn't do enough error checking on codegen phase
