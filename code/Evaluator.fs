module Evaluator

open AST

let rec to_string (expression: Expression) =
    match expression with
    | Number(n) -> string n
    | Variable(x) -> string x
    | Addition(es) -> failwith "TODO"
    | Multiplication(es) -> failwith "TODO"
    | Parentheses(e) -> failwith "TODO"
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
    | Parentheses(e) -> failwith "TODO"
    | Sequence(es) -> 
        printfn "Sequence should not be passed to simplify."
        exit 1

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
        | [] ->
            printfn "Invalid Sequence. There must be at least one expression."
            exit 1
    | _ -> 
        printfn "Top layer expression must be a sequence."
        exit 1