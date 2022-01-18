# Stack-Language-Interpreter
A final project for CS320, building a stack language interpreter
As a final project for my CS320 - Concept of Programming Language, I developed (with the help of course TA and professor) a functional stack language 
interpreter and compiler. 

All of the code written in part1_template.ml and part3_template.ml is written by myself with instructions and guidance from faculty members. 


# How it works

The compiler will take in basic commands such as Push, Pop, Let in (similar to how OCAML operates), If...then...else.
These commands input by the user will then be compiled from a high level-language into a low-level langauge by parsing through the source program.
The source program will be interpreted as a String.

Source program will look something like:

```
let rec fibo n =
  let rec eq x y =
    if x then
      if y then eq (x - 1) (y - 1) else 0 
    else
      if y then 0 else 1
  in
  if eq n 0 then 0 else
  if eq n 1 then 1 else
  fibo (n - 1) + fibo (n - 2)
in
fibo 10
```

This high-level language will then be compiled into low-level langauge, something like this:
```
Begin
Push fibo
Fun fibo n
Begin
Push eq
Fun eq x
Fun _ y
Push x
Lookup
If
Begin
Push y
Lookup
If
Begin
....
```

This low level language is a character stream, in which my program will take and create an Abstract Syntax Tree to be executed.
Using concepts such as Formal Grammars and Formal Semantics to be able to assign meanings to the character stream, the interpreter can execute the streams of characters and output the mathematical result.




