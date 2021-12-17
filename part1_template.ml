(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()
  
(* end of parser combinators *)

(*
GRAMMAR  -- this is the grammar of our interpreter
digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
nat ::= digit { digit }
letter ::= a...z | A...Z
initial:: = letter | _
name ::= initial{letter | digit| _ | '}
const ::= nat | name | ()


prog ::= coms
com ::= Push const | Trace
| Add | Sub | Mul | Div
| If coms Else coms End
coms ::= com { com }

val ::= nat | name | unit
 *)

type name = string


type const = Nat of int | Name of string | Unit | Func of (const * const * commands list * env)


and commands = Push of const | Add | Sub | Mul | Div | Trace | If of (commands list * commands list) 
|Let 
|Call
|Lookup
|Begin of commands list 
|Fun of (const * const * commands list)



and coms_list = commands list

and env = (const*const) list




(*environment is a data structure that associate a name with a value*)



(*first let build some const parser *)
let nat_parser : const parser = 
let*_ = ws in 
let* x = natural in 
let*_ = ws in
pure (Nat x)

let unit_parser : const parser = 
let*_ =ws in
let* x = keyword "()" >> pure (Unit) 
in pure x

let reserved = [
  "Push";
  "Add";
  "Sub";
  "Mul";
  "Div";
  "Trace";
  "Let";
  "If";
  "Else";
  "Fun";
  "End";
  "Let";
  "Begin";
  "Lookup"
]
let name : string parser =
  let* xs1 = many1 (satisfy (fun c -> is_alpha c || c = '_')) in
  let* xs2 = 
    many (satisfy (fun c ->
      is_alphanum c ||
      (c = '_') ||
      (c = '\'')))
  in
  let s = (implode xs1) ^ (implode xs2) in
  if List.exists (fun x -> x = s) reserved
  then fail
  else pure s << ws
 
let name_parser () =
  let* n = name in
  pure (Name n)

(*building a parser for Push and If, which process a string and convert it to commands representation *)
let parse_push = 
    let* _ = ws in
    (
    (let* x = nat_parser in pure (Push x))
    <|>
    (let* y = unit_parser in pure (Push y)))
    <|> 
    (let* z = name_parser () in pure (Push z))




(*built a command parser that parse string into individual commands, using mutually recursive function
individually parse each commands while calling a function.
This allow us to call If command recursively. 
*)
let rec command_parser () = 

    keyword "Push" >> parse_push
    <|> (keyword "Add" >> pure (Add))
    <|> (keyword "Sub" >> pure (Sub))
    <|> (keyword "Mul" >> pure (Mul))
    <|> (keyword "Div" >> pure (Div))
    <|> (keyword "Trace" >> pure (Trace))
    <|> (keyword "If" >>   (
                    let* _ = ws in
                    let* x =  (commands_parser()) in
                    let* y = keyword "Else" >> (commands_parser()) in
                    let* _ = ws in
                    let* _ = keyword "End" in pure (If (x,y))))
    <|> (keyword "Let" >> pure (Let)) 
    <|> (keyword "Lookup" >> pure (Lookup))
    <|> (keyword "Begin" >> (
          let* _ = ws in
          let* x = (commands_parser()) in 
          let* _ = ws in 
          let* _ = keyword "End" in
          pure (Begin x)
    ))
    <|> (keyword "Fun" >> (
      let* _ = ws in 
      let* fname = name_parser() in 
      let* _ = ws in
      let* args = name_parser() in
      let* _ = ws in 
      let* coms = commands_parser() in 
      let* _ = keyword "End" in
      pure (Fun (fname,args,coms)))
    )
    <|> (keyword "Call" >> pure (Call))
and commands_parser () = 
many' command_parser



(*--------------------------------------------------------------------------------------------*)

(*time for execution of the parser
input: current stack and a log (for Trace) 
output: option type, if None, then execution failure, else return updated stack * log
 *)

let rec push_stack (const: const) (inp_stack: const list) (log: string list) (envi: env): (const list*string list*env)option = 
    Some(const::inp_stack, log,envi)

and add_stack (inp_stack:const list) (log: string list) (envi: env) : (const list*string list*env) option =
match inp_stack with
[] -> None |
[x] -> None |
(Unit)::(Unit)::rest -> None |
(Unit)::(Nat v)::rest -> None |
(Nat v)::(Unit)::rest -> None |
(Nat v)::(Nat e)::rest -> Some ((Nat (e+v))::rest,log,envi)|
(_::_::rest) -> None

and sub_stack (inp_stack:const list) (log: string list) (envi: env) : (const list*string list*env) option =
match inp_stack with
[] -> None |
[x] -> None |
(Unit)::(Unit)::rest -> None |
(Unit)::(Nat v)::rest -> None |
(Nat v)::(Unit)::rest -> None |
(Nat v)::(Nat e)::rest -> Some ((Nat (e-v))::rest,log,envi) |
(_::_::rest) -> None


and mul_stack (inp_stack:const list) (log: string list) (envi:env) : (const list*string list*env) option =
match inp_stack with
[] -> None |
[x] -> None |
(Nat e)::(Nat v)::rest -> Some((Nat (e*v))::rest,log,envi)|
_::_::rest -> None

and div_stack (inp_stack:const list) (log: string list) (envi: env): (const list*string list*env) option =
match inp_stack with
[] -> None |
[x] -> None |
(Nat 0)::(Nat v)::rest -> None  |   (*divide by 0 *)
(Nat e)::(Nat v)::rest -> Some((Nat (v/e))::rest,log,envi)|
_::_::rest -> None



and convert_const_string (const : const) : string = match const with 
Nat v -> string_of_int v |
Unit -> "()"|
Name x -> x


and trace_stack (inp_stack:const list) (log: string list) (envi:env): (const list*string list*env) option =
match inp_stack with
[] -> None |
Func (fname,args,comm_list,envir)::rest -> Some(Unit::rest,"<fun>"::log,envi) |
top::rest -> Some(Unit::rest, (convert_const_string top)::log,envi)





(*use to compare string since == will not work
  "ka" == "ka" will return false *)
and compare_name (x : string) (y:string) : bool = match parse (keyword x) y with
  Some((),[]) -> true |
  Some((),char) -> false|
  None -> false
 



(*update function, which itereates through the given environment, and either:
    1. add a new association into the given environemnt
    2. replace a match association with new value
  done by iteresting through the environment, if not match, add to new_envi then recursively call 
  if match then replace (name, value) with updated value, then add to new environment and return.
   *)
and  update (Name var : const) (constant: const) (envi: env) (new_envi: env): env= match envi with 
  [] -> (Name var,constant)::new_envi |
  (Name var', val')::rest -> if (compare_name var' var) == true then 
                                        (Name var',constant)::(rest@new_envi)
                                        else call_update (Name var) (constant) rest ((Name var', val')::new_envi) 

and call_update (Name var) (constant) (envi) = 
  update (Name var) (constant) (envi) 


(*
consume 2 top element of stack
if 1st element = Nat and 2nd element = name, associate name with nat
return stack,log,environment
*)
and leT_stack (stack: const list) (log: string list) (envi: env) : (const list * string list * env) option = let ans = (match stack with
  v::[] -> None |
  [] -> None |
  (Nat v)::(Name var)::[] -> Some (update (Name var) (Nat v) envi [], [])|  (*consume 2 element, return rest of stack *)
  (Nat v)::(Name var)::rest -> Some (update (Name var) (Nat v) envi [], rest) |
  (Name x)::(Name y)::rest -> Some (update (Name y) (Name x) envi [], rest) |
  (Unit)::(Name y)::rest -> Some (update (Name y) (Unit) envi [], rest) |
  x::(Name y)::rest -> Some (update (Name y) x envi [], rest)|
  _::_::rest -> None) 
        in match ans with
        Some(new_envi,rest_stack) -> Some(rest_stack, log, new_envi) |
        None -> None




and help_lookup (Name var: const) (envi: env) = match envi with
        [] -> None |
        (Name var',value)::[] -> if (compare_name var var')==true then Some(value) else None|
        (Name var',value)::rest -> if (compare_name var var')==true then Some(value) else rec_help (Name var) rest 

and  rec_help (Name var) rest = 
  help_lookup (Name var) rest


and lookup_stack (stack: const list) (log: string list) (envi: env) : (const list * string list * env) option = 
  match stack with 
  [] -> None |
  (Name var)::rest -> (match help_lookup (Name var) envi with 
                        None -> None |
                        Some(value) -> Some((value::rest),log,envi))|
  _::rest -> None




(*mutually recursive function to process If command, allow us to process nested If statements *)
and  if_stack (if_path: commands list) (else_path: commands list) (inp_stack:const list) (log: string list) (envi:env) = 
  let x = (match inp_stack with 
        [] -> None |
        (Unit)::rest -> None |
        (Nat top)::rest -> if top <= 0 then execute else_path rest log envi else execute if_path rest log envi |
        _::rest -> None )
        in match x with 
                None -> None|
                Some(up_stack,up_log,evir) -> Some(up_stack,up_log,evir)



and begin_stack (coms_list: coms_list) (stack:const list) (log: string list) (envi:env): (const list * string list * env) option = 
  let copy_envi = create_copy envi in 
    match execute (coms_list) [] log copy_envi with 
      None -> None |
      Some (x::rest,inner_log,inner_envi) -> Some(x::stack,inner_log,envi) |
      Some([],inner_log,inner_envi) -> None

  

and create_copy (og_envi:env)  = List.fold_right (fun x y ->  x::y) og_envi []

and fun_stack (stack:const list) (log: string list) (envi:env) (fname: const) (args: const) (com_list: commands list)
: (const list * string list * env) option = 
let x = Func (fname, args, com_list , envi) in 
match fname with 
(Nat v) -> None | (*function name cannot be int *)
(Name fun_name) -> Some (stack, log , (fname,x)::envi)







and call_stack (stack: const list) (log: string list) (envi:env) : (const list * string list * env) option = 
match stack with
[] -> None |
[x] -> None|
v1::(Func (fname,args,com_list,envir))::rest -> 
                (match  execute com_list [] log ((fname,(Func (fname,args,com_list,envir)))::(args,v1)::envir) with
              None -> Some([],["function execution error"],[]) |
              Some(x::inner_rest,log,inner_envir) -> Some(x::rest,log,envi) |
              Some([],log,inner_envir) -> None
)|
v1::_::rest -> None (*second element on stack is not closure *)


(*--------------------------------------------------------------------------------*)
and execute_command (command: commands) (stack: const list) (log:string list) (envi:env)= let ans = (match command with
Push x -> push_stack x stack log envi|
Add -> add_stack stack log envi|
Sub -> sub_stack stack log envi|
Mul -> mul_stack stack log envi|
Div -> div_stack stack log envi|
Trace -> trace_stack stack log envi|
If (if_path,else_path) -> if_stack if_path else_path stack log envi|
Let -> leT_stack stack log envi |
Lookup -> lookup_stack stack log envi |
Begin com_list ->  begin_stack com_list stack log envi |
Fun (fname,arg,com_list) -> fun_stack stack log envi fname arg com_list|
Call -> call_stack stack log envi )
in ans 


and execute (coms_list : commands list) (stack: const list) (log: string list) (envi:env)= match coms_list with 
    [] -> Some(stack,log, envi) |
    coms::rest ->  match execute_command coms stack log envi with
                None -> None |
                Some(update_stack, up_log,envir) -> call_execute rest update_stack up_log envir

and call_execute (coms_list : commands list) (stack: const list) (log: string list) (envi:env) = 
    execute coms_list stack log envi



let test_execute (str:string) = match parse (commands_parser()) str with 
  None -> ("error while parsing",[],[],[])|
  Some(comds_list,[]) -> (match execute (comds_list) [] [] [] with 
          None -> ("error while executing",[],[],[]) |
          Some(update_stack, log, envi) -> ("success: stack, log, envi", update_stack,log,envi))|
  Some(comds_list,rest) -> ("error while parsing, wrong command",[],[],[])


let interpreter (src :string) : string * string list = match parse (commands_parser ()) src with
    None -> ("Error", []) |
    Some(comds_list, []) -> (match execute (comds_list) [] [] [] with
                            None -> ("Error",[]) |
                            Some(x::rest,log,envi) -> (convert_const_string x,log))|
    Some(comds_list,rest) -> ("Error",[])




(*Explanation *)

(*

1. the interpreter function will call commands_parser, which will translate string into internal representation (Nat int, Push,....)
 - if succeed, commands_parser will return a list of commands for execution
 - else, commands_parser will return None

 - commands_parser rely heavily on mutually recursive to recursively process nested If statements


2. the interpreter will now pass the list of commands to another function call execute
 - the execute function will traverse the list of commands:
    - pass each command into a function called execute_command
    - execute_command will execute the individual command, returning either success or error
          - to process If statements, execute_command need to call a specialise function called if_stack 
                - if_stack is a mutually recursive function which decide which branch of if to exececute
                - if stack process the entire branch, returning the final stack and log
                - returning final stack * log just like any other command

  - after traversing the entire list of commands:
    - if execution is successful, execute will return the final stack (still in internal representation form)
    - if during execution, something fail, then execute will return error string

3. the interpreter now receive the final stack list (in internal representation format)
  - if error, then interpreter will output error and log
  - if no error occur during parsing or executing:
    - interpreter translate the final stack to a string list
    - return string list
  - OUTPUT string list

 *)



