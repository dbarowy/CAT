module Evaluator

open AST
open Simplifier

let floating_point_char = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "."]

let rec begins_with (prefixes: string list) (s: string) =
    match prefixes with
    | prefix::prefixes -> s.StartsWith(prefix) || begins_with prefixes s
    | _ -> false

let rec ends_with (suffixes: string list) (s: string) =
    match suffixes with
    | suffix::suffixes -> s.EndsWith(suffix) || ends_with suffixes s
    | _ -> false

// Returns whether or not this type of expression should be parenthesized
// if it is part of a more complicated expression
let needs_parens expr =
    match expr with
    | Number n when n >= 0 -> false
    | Variable _ -> false
    // | Exponentiation _ -> false (Stylistic difference)
    | _ -> true 

// Returns a pretty string representation of the expression
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
        | e::es -> 
            let s = if needs_parens e then "(" + to_string e + ")" else to_string e
            let tail = to_string (Multiplication es)
            if (ends_with floating_point_char s && begins_with floating_point_char tail) then
                "(" + s + ")" + to_string (Multiplication es)
            else
                s + to_string (Multiplication es)
        | [] -> ""
    | Exponentiation(e1, e2) -> 
        // Check if we need to wrap either part in parentheses
        match needs_parens e1, needs_parens e2 with
        | true, true ->
            "(" + (to_string e1) + ")^(" + (to_string e2) + ")"
        | true, false ->
            "(" + (to_string e1) + ")^" + (to_string e2)
        | false, true ->
            (to_string e1) + "^(" + (to_string e2) + ")"
        | false, false ->
            (to_string e1) + "^" + (to_string e2)
            
    | Sequence(es) -> 
        "[" +
            match es with 
            | [e] -> (to_string e) + "]"
            | e::es -> (to_string e) + ", " + (to_string (Sequence es))
            | [] -> ""

let process_expression (expression: Expression) =
    printfn "Simplifying: %s" (to_string expression)
    let expressions = (reorder_terms (flatten_ast expression))::(simplify expression)
    // printfn "%A" (expressions)
    
    // Print out each subsequent simplification and return the final version
    List.fold
        (fun _ e -> 
            printfn "==> %s" (to_string e)
            e)
        expression // This is a dummy placeholder
        expressions

let rec evaluate (expression: Expression) =
    match expression with
    | Sequence(es) -> 
        match es with 
        | [e] ->
            [process_expression e]
        | e::es -> 
            let expr = process_expression e
            printfn ""
            expr::(evaluate (Sequence es))
        | [] -> failwith "Invalid Sequence. There must be at least one expression."
    | _ -> failwith "Top layer expression must be a sequence."