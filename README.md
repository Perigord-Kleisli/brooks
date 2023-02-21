# Brooks
A simple lambda calculus interpreter

`Note: this is not thoroughly tested, Alpha conversion may not
be guaranteed to be correct.`

## Usage
```
Usage: brooks [OPTION...] files...
    --parse      Show the syntax tree
    --reduction  Do a β reduction
    --repl       Enable the REPL
```

## Repl
```
<expr>                          Evaluate an expression
:b              <expr>          Do a beta reduction
:h                              Print Help
:q              <expr>          Quit
:p              <expr>          Print the generated syntax tree
:!              <cmd>           Run a shell command
```

## Samples
`λ and \ can be used interchangeably`
* ID 
```
λ> λx.x
1
```
* Church Numerals
```
λ> λf x. f (f x)
2
```
>  (Also supports just placing the number)
```
λ> \x. 10 x
10
```
* Beta Reduction 
```
λ> :b (\x.(\f.\x.f x)(\y.x)) (\a.a)(\b.b)
λa.a
```
* Get syntax tree
```
λ> :p \x.x

(Abstraction
  (Identifier x)
  (Identifier x))
```

# Test it out with flakes 
```sh
nix run github:Trouble-Truffle/brooks -- --repl
```
