(* labject123.ml - Optimized Version *)

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
  
  let rec eval env = function
    | Cons(func, args) ->
        let evaluated_func = eval env func in
        (match evaluated_func with
         | Primitive f -> f args env
         | Closure(params, body, closure_env) ->
             let rec bind_params ps as' env =
               match ps, as' with
               | Nil, Nil -> env
               | Cons(Symbol p, ps), Cons(a, as') ->
                   let evaluated_arg = eval env a in
                   bind_params ps as' ((p, evaluated_arg) :: env)
               | _ -> oops "Parameter/argument mismatch"
             in
             let new_env = bind_params params args closure_env in
             eval new_env body  (* Tail call optimized *)
         | _ -> oops "Not a function")
    | Symbol s -> 
        (try List.assoc s env 
         with Not_found -> oops ("Unbound symbol: " ^ s))
    | x -> x

  let evaluate thing = eval [] thing
end

module Scanner =
struct
  (* ... Your existing scanner implementation ... *)
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
    | Scanner.OpenParenToken -> nextToken(); nextThings []
    | Scanner.SymbolToken "nil" -> nextToken(); Nil
    | Scanner.SymbolToken s -> nextToken(); Symbol s

  and nextThings acc =
    match !token with
    | Scanner.CloseParenToken -> nextToken(); List.rev acc
    | Scanner.EndToken -> raise (Can'tParse "Unclosed list")
    | _ ->
        let head = nextThing() in
        nextThings (head :: acc)

  let nextThings() =
    match nextThings [] with
    | [] -> Nil
    | hd::tl -> List.fold_right (fun x acc -> Cons(x, acc)) tl hd
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

  let printThing thing =
    let rec printingThing = function
      | Closure _ -> printf "[Closure]"
      | Primitive _ -> printf "[Primitive]"
      | Number n -> printf "%i" n
      | Symbol s -> printf "%s" s
      | Nil -> printf "nil"
      | Cons(car, cdr) ->
          printf "(";
          let rec print_cons = function
            | Cons(h, t) ->
                printingThing h;
                (match t with
                 | Nil -> ()
                 | _ -> printf " "; print_cons t)
            | Nil -> ()
            | x -> printf " . "; printingThing x
          in
          printingThing car;
          print_cons cdr;
          printf ")"
    in
    try printingThing thing; printf "\n"
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
      let rec loop () =
        try
          let expr = Parser.nextThing() in
          if expr = Symbol "end" then ()
          else
            let result = Evaluator.evaluate expr in
            Printer.printThing result;
            loop ()
        with
        | Evaluator.EvaluatorError msg ->
            Printf.printf "%s: Evaluator error %s\n%!" filename msg;
            loop ()
        | Parser.Can'tParse msg ->
            Printf.printf "%s: Parser error %s\n%!" filename msg;
            loop ()
        | Printer.BadThing ->
            Printf.printf "%s: Printer error\n%!" filename;
            loop ()
        | exn ->
            Printf.printf "%s: Internal error\n%!" filename;
            loop ()
      in
      loop ()
    with
    | Sys_error msg -> Printf.printf "File error: %s\n%!" msg
    | exn -> Printf.printf "%s: Initialization error\n%!" filename

  let repl() = commandArguments handle_file
end

let () = Lisp.repl ()
