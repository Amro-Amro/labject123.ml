type thing = 
  | Closure of thing * thing * environment 
  | Cons of thing * thing 
  | Nil 
  | Number of int 
  | Primitive of (thing -> environment -> thing) 
  | Symbol of string 
and environment = (string * thing) list

(* ======================== EVALUATOR ======================== *)
module type Evaluatish =
sig
  val evaluate : thing -> thing
  exception EvaluatorError of string
end ;;

module Evaluator : Evaluatish = struct
  exception EvaluatorError of string
  let evaluate _ = (* ... your implementation ... *)
end ;;
module Evaluator : Evaluatish = struct
  exception EvaluatorError of string
  let oops msg = raise (EvaluatorError msg)
  
  (* ENVIRONMENT HANDLING *)
  let rec envGet env name etc =
    let rec envGetting = function
      | [] -> etc ()
      | (n,v)::t -> if n = name then v else envGetting t
    in envGetting env

  let envMake () = []
  let envPut name value env = (name, value) :: env
  let tee = Symbol "t"
  let global = ref (envMake ())
  
  let () = 
    global := envPut "nil" Nil !global;
    global := envPut "t" tee !global

  let lookup env name =
    envGet env name (fun () ->
      envGet !global name (fun () ->
        oops ("Unbound name " ^ name)))

  (* CORE EVALUATION LOGIC *)
  let rec evaluating thing env = 
    match thing with
    | Cons(func, args) ->
        (match evaluating func env with
         | Closure(params, body, closureEnv) ->
             let rec apply params args env =
               match params, args with
               | Nil, Nil -> evaluating body env
               | Cons(Symbol p, ps), Cons(a, as') ->
                   let argVal = evaluating a env
                   in apply ps as' (envPut p argVal env)
               | _ -> oops "Bad application"
             in apply params args closureEnv
         | Primitive f -> f args env
         | _ -> oops "Not a function")
    | Symbol name -> lookup env name
    | _ -> thing

  let evaluate thing = evaluating thing (envMake ())

  (* PRIMITIVE DEFINITIONS *)
  let makeArithmetic op msg = fun args env ->
    match args with
    | Cons(left, Cons(right, Nil)) ->
        let l = evaluating left env in
        let r = evaluating right env in
        (match l, r with
         | Number l, Number r -> Number (op l r)
         | _ -> oops msg)
    | _ -> oops msg

  let makeRelation op msg = fun args env ->
    match args with
    | Cons(left, Cons(right, Nil)) ->
        let l = evaluating left env in
        let r = evaluating right env in
        (match l, r with
         | Number l, Number r -> if op l r then tee else Nil
         | _ -> oops msg)
    | _ -> oops msg
  
  (* PRIMITIVE FUNCTIONS *)
   let primitive name howTo = global := envPut name (Primitive howTo) !global

  let () =
    primitive "*" (makeArithmetic ( * ) "* expected two numbers");
    primitive "+" (makeArithmetic ( + ) "+ expected two numbers");
    primitive "-" (makeArithmetic ( - ) "- expected two numbers");
    primitive "/" (makeArithmetic ( / ) "/ expected two numbers");
    primitive "<" (makeRelation (<) "< expected two numbers");
    primitive "<=" (makeRelation (<=) "<= expected two numbers");
    primitive "<>" (makeRelation (<>) "<> expected two numbers");
    primitive ">" (makeRelation (>) "> expected two numbers");
    primitive ">=" (makeRelation (>=) ">= expected two numbers");

    primitive "=" (fun args env ->
      match args with
      | Cons(left, Cons(right, Nil)) ->
          let l = evaluating left env in
          let r = evaluating right env in
          (match l, r with
          | Nil, Nil -> tee
          | Number l, Number r when l = r -> tee
          | Symbol l, Symbol r when l = r -> tee
          | _ -> Nil)
      | _ -> oops "= expected two arguments");

  primitive "and" (fun args env ->
    let rec anding = function
      | Nil -> tee
      | Cons(arg, Nil) -> evaluating arg env
      | Cons(arg, rest) ->
          if evaluating arg env = Nil then Nil
          else anding rest
      | _ -> oops "AND expected list" 
    in anding args);

  primitive "car" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Cons(first, _) -> first
         | _ -> oops "CAR expected CONS")
    | _ -> oops "CAR expected one argument");

  primitive "cdr" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Cons(_, rest) -> rest
         | _ -> oops "CDR expected CONS")
    | _ -> oops "CDR expected one argument");

  primitive "cons" (fun args env ->
    match args with
    | Cons(first, Cons(rest, Nil)) ->
        let f = evaluating first env in
        let r = evaluating rest env in
        Cons(f, r)
    | _ -> oops "CONS expected two arguments");

  primitive "define" (fun args env ->
    match args with
    | Cons(Symbol name, Cons(value, Nil)) ->
        let v = evaluating value env in
        global := envPut name v !global;
        Symbol name
    | _ -> oops "DEFINE expected symbol and value");

  primitive "if" (fun args env ->
    match args with
    | Cons(test, Cons(then_expr, Cons(else_expr, Nil))) ->
        if evaluating test env = Nil
        then evaluating else_expr env
        else evaluating then_expr env
    | _ -> oops "IF expected three arguments");

  primitive "is-cons" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Cons _ -> tee | _ -> Nil)
    | _ -> oops "IS-CONS expected one argument");

  primitive "is-function" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Primitive _ | Closure _ -> tee | _ -> Nil)
    | _ -> oops "IS-FUNCTION expected one argument");

  primitive "is-number" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Number _ -> tee | _ -> Nil)
    | _ -> oops "IS-NUMBER expected one argument");

  primitive "is-symbol" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        (match evaluating arg env with
         | Symbol _ | Nil -> tee | _ -> Nil)
    | _ -> oops "IS-SYMBOL expected one argument");

  primitive "lambda" (fun args env ->
    match args with
    | Cons(params, Cons(body, Nil)) ->
        let rec valid_params = function
          | Nil -> true
          | Cons(Symbol _, rest) -> valid_params rest
          | _ -> false
        in if valid_params params 
        then Closure(params, body, env)
        else oops "LAMBDA invalid parameters"
    | _ -> oops "LAMBDA expected parameter list and body");

  primitive "list" (fun args env ->
    let rec build_list = function
      | Nil -> Nil
      | Cons(h, t) -> Cons(evaluating h env, build_list t)
      | _ -> oops "LIST invalid arguments"
    in build_list args);

  primitive "not" (fun args env ->
    match args with
    | Cons(arg, Nil) ->
        if evaluating arg env = Nil then tee else Nil
    | _ -> oops "NOT expected one argument");

  primitive "or" (fun args env ->
    let rec oring = function
      | Nil -> Nil
      | Cons(arg, rest) ->
          let value = evaluating arg env in
          if value <> Nil then value else oring rest
      | _ -> oops "OR invalid arguments"
    in oring args);

  primitive "quote" (fun args _ ->
    match args with
    | Cons(arg, Nil) -> arg
    | _ -> oops "QUOTE expected one argument");
end

(* ======================== SCANNER ======================== *)
module Scanner : Scannerish = struct
  type token =
    | CloseParenToken
    | EndToken
    | NumberToken of int
    | OpenParenToken
    | SymbolToken of string

  let input = ref stdin
  let ch = ref ' '

  let nextChar () =
    try ch := input_char !input
    with End_of_file -> ch := '\000'

  let initialize path = 
    input := open_in path;
    nextChar ()

  let rec nextComment () =
    match !ch with
    | '\000' | '\n' -> nextChar ()
    | _ -> nextChar (); nextComment ()

  let rec nextToken () =
    match !ch with
    | '\000' -> EndToken
    | ' ' | '\t' | '\n' -> nextChar (); nextToken ()
    | '(' -> nextChar (); OpenParenToken
    | ')' -> nextChar (); CloseParenToken
    | ';' -> nextComment (); nextToken ()
    | '0'..'9' | '-' ->
        let buffer = Buffer.create 16 in
        Buffer.add_char buffer !ch;
        nextChar ();
        while !ch >= '0' && !ch <= '9' do
          Buffer.add_char buffer !ch;
          nextChar ()
        done;
        NumberToken (int_of_string (Buffer.contents buffer))
    | _ ->
        let buffer = Buffer.create 16 in
        Buffer.add_char buffer !ch;
        nextChar ();
        while match !ch with
              | ' ' | '\t' | '\n' | '(' | ')' | '\000' -> false
              | _ -> true
        do
          Buffer.add_char buffer !ch;
          nextChar ()
        done;
        SymbolToken (Buffer.contents buffer)
end

(* ======================== PARSER (PROJECT 2) ======================== *)
module type Parsish =
sig
  exception Can'tParse of string
  val initialize : string -> unit
  val nextThing : unit -> thing
end

module Parser : Parsish = struct
  exception Can'tParse of string
  let token = ref (Scanner.EndToken)

  let initialize path =
    Scanner.initialize path;
    token := Scanner.nextToken ()

  let nextToken () = 
    token := Scanner.nextToken ()

  let rec nextThing () =
    match !token with
    | Scanner.CloseParenToken -> 
        raise (Can'tParse "Unexpected CloseParenToken")
    | Scanner.EndToken -> 
        raise (Can'tParse "Unexpected EndToken")
    | Scanner.NumberToken n ->
        nextToken ();
        Number n
    | Scanner.OpenParenToken ->
        nextToken ();
        let list = nextThings () in
        list
    | Scanner.SymbolToken "nil" ->
        nextToken ();
        Nil
    | Scanner.SymbolToken s ->
        nextToken ();
        Symbol s

  and nextThings () =
    match !token with
    | Scanner.CloseParenToken ->
        nextToken ();
        Nil
    | Scanner.EndToken ->
        raise (Can'tParse "List not closed")
    | _ ->
        let head = nextThing () in
        let tail = nextThings () in
        Cons (head, tail)
end

(* ======================== PRINTER (LAB 9) ======================== *)
module type Printish =
sig
  exception BadThing
  val printThing : thing -> unit
end

module Printer : Printish = struct
  open Printf
  exception BadThing

  let rec is_list = function
    | Nil -> true
    | Cons (_, t) -> is_list t
    | _ -> false

  let rec printingThing thing =
    match thing with
    | Closure _ -> printf "[Closure]"
    | Primitive _ -> printf "[Primitive]"
    | Number n -> printf "%i" n
    | Symbol s -> printf "%s" s
    | Nil -> printf "nil"
    | Cons (car, cdr) ->
        let print_list () =
          printf "(";
          let rec helper first = function
            | Cons (h, t) ->
                (if not first then printf " ");
                printingThing h;
                helper false t
            | Nil -> printf ")"
            | _ -> 
                printf " . ";
                printingThing cdr;
                printf ")"
          in
          printingThing car;
          helper true cdr
        in
        if is_list cdr then print_list ()
        else (
          printf "(";
          printingThing car;
          printf " . ";
          printingThing cdr;
          printf ")")

  let printThing thing =
    printingThing thing;
    printf "\n"
end

(* ======================== LISP INTERPRETER ======================== *)
module type Lispish =
sig
  val repl : unit -> unit
end

module Lisp : Lispish = struct
  let commandArguments etc =
    Arg.parse
      [ ("", (Arg.String (fun _ -> ())), "Zero or more arguments expected") ]
      etc
      "Argument expected"

  let repl () =
    let process_file filename =
      try
        Parser.initialize filename;
        let rec loop () =
          try
            let expr = Parser.nextThing () in
            if expr = Symbol "end" then ()
            else
              let result = Evaluator.evaluate expr in
              Printer.printThing result;
              loop ()
          with
          | Evaluator.EvaluatorError s ->
              Printf.printf "%s: Evaluator error %s\n" filename s
          | Parser.Can'tParse s ->
              Printf.printf "%s: Parser error %s\n" filename s
          | Printer.BadThing ->
              Printf.printf "%s: Printer error\n" filename
          | _ ->
              Printf.printf "%s: Internal error\n" filename
        in loop ()
      with _ ->
        Printf.printf "%s: File initialization error\n" filename
    in
    commandArguments process_file
end

(* ======================== MAIN ENTRY POINT ======================== *)
let () = Lisp.repl ()
