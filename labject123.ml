type thing =
  | Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list

module type Parsish =
sig
  exception Can'tParse of string
  val initialize : string -> unit
  val nextThing : unit -> thing
end

module Parser: Parsish =
struct
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
    | Cons (_, t) -> is_list t
    | _ -> false

  let rec printingThing thing =
    match thing with
    | Closure _ -> printf "[Closure]"
    | Primitive _ -> printf "[Primitive]"
    | Number n -> printf "%i" n
    | Symbol s -> printf "%s" s
    | Nil -> printf "nil"
    | Cons _ ->
        if is_list thing then (
          printf "(";
          printingThings thing;
          printf ")"
        ) else (
          let rec print_dotted = function
            | Cons (a, b) ->
                printingThing a;
                (match b with
                | Nil -> ()
                | Cons _ -> printf " "; print_dotted b
                | _ -> printf " . "; printingThing b)
            | _ -> ()
          in
          printf "(";
          print_dotted thing;
          printf ")"
        )

  and printingThings things =
    match things with
    | Nil -> ()
    | Cons (car, Nil) -> printingThing car
    | Cons (car, cdr) ->
        printingThing car;
        printf " ";
        printingThings cdr
    | _ -> printingThing things

  let printThing thing =
    printingThing thing;
    printf "\n"
end

module type Lispish =
sig
  val repl : unit -> unit
end

(* LISP. Read Lisp programs from files and execute them. *)
module Lisp: Lispish =
struct
  let commandArguments etc =
    Arg.parse
      [ ("", (Arg.String (fun _ -> ())), "Zero or more arguments expected") ]
      etc
      "Argument expected"

  let repl () =
    let handle_file filename =
      try
        Parser.initialize filename;
        let rec loop () =
          let expr = Parser.nextThing () in
          match expr with
          | Symbol "end" -> ()
          | _ ->
              let value = Evaluator.evaluate expr in
              Printer.printThing value;
              loop ()
        in loop ()
      with
      | Evaluator.EvaluatorError s ->
          Printf.printf "%s: Evaluator error %s\n" filename s
      | Parser.Can'tParse s ->
          Printf.printf "%s: Parser error %s\n" filename s
      | Printer.BadThing ->
          Printf.printf "%s: Printer error\n" filename
      | _ ->
          Printf.printf "%s: Internal error\n" filename
    in
    commandArguments handle_file
end
