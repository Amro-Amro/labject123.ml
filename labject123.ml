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
end

module type Scannerish =
sig
  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string
  val initialize: string -> unit
  val nextToken: unit -> token
end

module Scanner: Scannerish =
struct
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
    | Number n -> printf "%d" n
    | Symbol s -> printf "%s" s
    | Nil -> printf "nil"
    | Cons _ ->
        if is_list thing then (
          printf "(";
          let rec printingThings = function
            | Nil -> ()
            | Cons(h, Nil) -> printingThing h
            | Cons(h, t) -> printingThing h; printf " "; printingThings t
            | _ -> raise BadThing
          in
          printingThings thing;
          printf ")")
        else (
          let rec print_dotted = function
            | Cons(a, b) ->
                printingThing a;
                (match b with
                | Nil -> ()
                | Cons _ -> printf " "; print_dotted b
                | _ -> printf " . "; printingThing b)
            | _ -> raise BadThing
          in
          printf "(";
          print_dotted thing;
          printf ")")

  let printThing thing =
    try printingThing thing; print_newline()
    with _ -> raise BadThing
end

module type Lispish =
sig
  val repl : unit -> unit
end

module Lisp: Lispish =
struct
  let commandArguments etc =
    Arg.parse [] etc "Lisp interpreter"

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
            Printf.printf "%s: Eval error - %s\n" filename msg; loop()
        | Parser.Can'tParse msg ->
            Printf.printf "%s: Parse error - %s\n" filename msg; loop()
        | Printer.BadThing ->
            Printf.printf "%s: Print error\n" filename; loop()
        | _ ->
            Printf.printf "%s: Unknown error\n" filename; loop()
      in
      loop()
    with
    | Sys_error msg -> Printf.printf "File error: %s\n" msg
    | _ -> Printf.printf "%s: Initialization error\n" filename

  let repl() = commandArguments handle_file
end

let () = Lisp.repl ()
