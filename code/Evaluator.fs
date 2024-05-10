module Evaluator

open AST

let floating_point_char = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "."]

let rec begins_with (prefixes: string list) (s: string) =
    match prefixes with
    | prefix::prefixes -> s.StartsWith(prefix) || begins_with prefixes s
    | _ -> false

let rec ends_with (suffixes: string list) (s: string) =
    match suffixes with
    | suffix::suffixes -> s.EndsWith(suffix) || ends_with suffixes s
    | _ -> false


let rec to_string (expression: Expression) =
    match expression with
    | Number(n) -> string n
    | Variable(x) -> string x
    | Addition(es) -> 
        match es with
        | [e] -> to_string e
        | e::es -> (to_string e) + " + " + (to_string (Addition es))
        | [] -> failwith "Invalid Addition"
    | Multiplication(es) -> 
        // Check whether or not we can write it as an implicit multiplication
        match es with
        | [e] -> to_string e
        | e::es -> 
            let s = to_string e
            let tail = to_string (Multiplication es)
            if begins_with ["-"] s || (ends_with floating_point_char s && begins_with floating_point_char tail) then
                "(" + s + ")" + to_string (Multiplication es)
            else
                s + to_string (Multiplication es)
        | [] -> failwith "Invalid Multiplication"
    | Exponentiation(e1, e2) -> 
        // Check if we need to wrap the exponent in parentheses
        match e2 with
        | Number(n) when n >= 0 -> (to_string e1) + "^" + (to_string e2)
        | Variable(_) -> (to_string e1) + "^" + (to_string e2)
        | _ -> (to_string e1) + "^(" + (to_string e2) + ")"
    | Parentheses(e) -> "(" + (to_string e) + ")"
    | Sequence(es) -> 
        "[" +
            match es with 
            | [e] -> (to_string e) + "]"
            | e::es -> (to_string e) + ", " + (to_string (Sequence es))
            | [] -> ""

(*
 * @return A list of progressively more simplified versions of the expression
 *)
let rec simplify (expression: Expression) =
    match expression with
    | Number(n) ->
        // Maximally simplified already
        [expression]
    | Variable(x) -> 
        // Maximally simplified
        [expression]
    | Addition(es) -> failwith "TODO"
    | Multiplication(es) -> failwith "TODO"
    | Exponentiation(expbase, exponent) -> failwith "TODO"
    | Parentheses(e) -> failwith "TODO"
    | Sequence(es) -> failwith "Sequence should not be passed to simplify."

let process_expression (expression: Expression) =
    printfn "Simplifying: %s" (to_string expression)
    let expressions = simplify expression
    
    List.fold
        (fun _ e -> 
            printfn "==> %s" (to_string e))
        ()
        expressions
    ()

let rec evaluate (expression: Expression) =
    match expression with
    | Sequence(es) -> 
        match es with 
        | [e] ->
            process_expression e
        | e::es -> 
            process_expression e
            printfn ""
            evaluate (Sequence es)
        | [] -> failwith "Invalid Sequence. There must be at least one expression."
    | _ -> failwith "Top layer expression must be a sequence."