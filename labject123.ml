(* labject123.ml - Final Corrected Version *)

type thing = 
  | Closure of thing * thing * environment 
  | Cons of thing * thing 
  | Nil 
  | Number of int 
  | Primitive of (thing -> environment -> thing) 
  | Symbol of string 
and environment = (string * thing) list

module type Evaluatish =
sig
  val evaluate: thing -> thing
  exception EvaluatorError of string
end

module Evaluator: Evaluatish =
struct
  exception EvaluatorError of string
  let oops msg = raise (EvaluatorError msg)
  
  let rec eval thing env =
    match thing with
    | Cons(func, args) ->
        let evaluated_func = eval func env in
        (match evaluated_func with
         | Primitive f -> f args env
         | Closure(params, body, closure_env) ->
             let rec bind_params params args env =
               match params, args with
               | Nil, Nil -> env
               | Cons(Symbol p, ps), Cons(a, as') ->
                   let evaluated_arg = eval a env in
                   bind_params ps as' ((p, evaluated_arg) :: env)
               | _ -> oops "Parameter/argument mismatch"
             in
             let new_env = bind_params params args closure_env in
             eval body new_env
         | _ -> oops "Not a function")
    | Symbol s -> 
        (try List.assoc s env 
         with Not_found -> oops ("Unbound symbol: " ^ s))
    | x -> x

  let evaluate thing = eval thing []
end

module Scanner =
struct
  type token =
    | CloseParenToken
    | EndToken
    | NumberToken of int
    | OpenParenToken
    | SymbolToken of string

  let ch = ref ' '
  let input = ref stdin

  let initialize path =
    input := open_in path;
    ch := input_char !input

  let rec nextToken() =
    match !ch with
    | ' ' | '\t' | '\n' -> 
        (while List.mem !ch [' '; '\t'; '\n'] do 
           ch := input_char !input 
         done; 
         nextToken())
    | '(' -> ch := input_char !input; OpenParenToken
    | ')' -> ch := input_char !input; CloseParenToken
    | '0'..'9' | '-' ->
        let buffer = Buffer.create 16 in
        Buffer.add_char buffer !ch;
        (try while true do
           ch := input_char !input;
           if !ch >= '0' && !ch <= '9' 
           then Buffer.add_char buffer !ch
           else raise Exit
         done with Exit -> ());
        NumberToken (int_of_string (Buffer.contents buffer))
    | _ ->
        let buffer = Buffer.create 16 in
        while !ch <> ' ' && !ch <> '\t' && !ch <> '\n' &&
              !ch <> '(' && !ch <> ')' && !ch <> ';' do
          Buffer.add_char buffer !ch;
          ch := input_char !input
        done;
        SymbolToken (Buffer.contents buffer)
end

module type Parsish =
sig
  exception Can'tParse of string
  val initialize : string -> unit
  val nextThing : unit -> thing
end

module Parser: Parsish =
struct
  exception Can'tParse of string
  let token = ref Scanner.EndToken

  let initialize path =
    Scanner.initialize path;
    token := Scanner.nextToken()

  let nextToken() = token := Scanner.nextToken()

  let rec nextThing() = 
    match !token with
    | Scanner.CloseParenToken -> raise (Can'tParse "Unexpected )")
    | Scanner.EndToken -> raise (Can'tParse "Unexpected EOF")
    | Scanner.NumberToken n -> nextToken(); Number n
    | Scanner.OpenParenToken -> nextToken(); nextThings()
    | Scanner.SymbolToken "nil" -> nextToken(); Nil
    | Scanner.SymbolToken s -> nextToken(); Symbol s

  and nextThings() =
    match !token with
    | Scanner.CloseParenToken -> nextToken(); Nil
    | Scanner.EndToken -> raise (Can'tParse "Unclosed list")
    | _ -> Cons(nextThing(), nextThings())
end

module type Printish =
sig
  exception BadThing
  val printThing : thing -> unit
end

module Printer: Printish =
struct
  open Printf
  exception BadThing

  let rec is_list = function
    | Nil -> true
    | Cons(_, t) -> is_list t
    | _ -> false

  let rec printingThing thing =
    match thing with
    | Closure _ -> printf "[Closure]"
    | Primitive _ -> printf "[Primitive]"
    | Number n -> printf "%i" n
    | Symbol s -> printf "%s" s
    | Nil -> printf "nil"
    | Cons(car, cdr) ->
        if is_list (Cons(car, cdr)) then (
          printf "(";
          let rec print_list = function
            | Cons(h, t) ->
                printingThing h;
                (match t with
                 | Nil -> ()
                 | _ -> printf " "; print_list t)
            | Nil -> ()
          in
          print_list (Cons(car, cdr));
          printf ")")
        else (
          printf "(";
          printingThing car;
          printf " . ";
          printingThing cdr;
          printf ")")

  let printThing thing =
    try
      printingThing thing;
      printf "\n"
    with _ -> raise BadThing
end

module type Lispish =
sig
  val repl : unit -> unit
end

module Lisp: Lispish =
struct
  let commandArguments etc =
    Arg.parse [] (fun s -> etc s) "Lisp interpreter"

  let handle_file filename =
    try
      Parser.initialize filename;
      let rec loop() =
        try
          let expr = Parser.nextThing() in
          if expr = Symbol "end" then ()
          else
            let result = Evaluator.evaluate expr in
            Printer.printThing result;
            loop()
        with
        | Evaluator.EvaluatorError msg ->
            Printf.printf "%s: Evaluator error %s\n" filename msg; loop()
        | Parser.Can'tParse msg ->
            Printf.printf "%s: Parser error %s\n" filename msg; loop()
        | Printer.BadThing ->
            Printf.printf "%s: Printer error\n" filename; loop()
        | _ ->
            Printf.printf "%s: Internal error\n" filename; loop()
      in
      loop()
    with
    | Sys_error msg -> Printf.printf "File error: %s\n" msg
    | _ -> Printf.printf "%s: Initialization error\n" filename

  let repl() = commandArguments handle_file
end

let () = Lisp.repl ()
