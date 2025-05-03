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
  val evaluate: thing -> thing
  exception EvaluatorError of string
end

module Evaluator : Evaluatish = struct
  (* ... [Keep original Evaluator implementation exactly as provided] ... *)
end

(* ======================== SCANNER ======================== *)
module Scanner : Scannerish = struct
  (* ... [Keep original Scanner implementation exactly as provided] ... *)
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
