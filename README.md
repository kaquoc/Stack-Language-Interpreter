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


# How to run
First and foremost, the program is divided into two file, part1_template.ml which is our interpreter and part3_template.ml which is our compiler.
To run Ocaml, we need to download:
* https://ocaml.org/docs/install.html
Our if using Visual Studio, we can install the following extension:
* https://marketplace.visualstudio.com/items?itemName=freebroccolo.reasonml

In the example folder, the out folder contains text1.txt...text2.txt which is the inputs for our interpreter (part1_template.ml). Executing the content of the text file into the interpreter would then return output contain in the res folder.

Example:

Executing the text1.txt file in out folder would return the text1.txt in res folder.

The src folder contains the inputs for our compiler, the corresponding output is the input for our interpreter. Meaning:

Executing the text1.txt file in src folder would return the text1.txt file in out folder.

# Final thought
The semester is over, and I will not continue any changes to the codes. All text file in the example folder works and has been tested by myself. I do intend to revisit my work from time to time. Any code suggestion and changes are welcomed and I'm eager to learn from my mistake.

# Note 11/30/2021
I do intend to revisit this readme.md and fix some of my wordings.





