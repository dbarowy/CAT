module Evaluator

open AST

type Environment = Map<char, Expression>

let rec to_string (expression: Expression) =
    match expression with
    | Number(n) -> string n
    | Variable(x) -> string x
    | Assignment(e1, e2) -> (to_string e1) + " = " + (to_string e2)
    | Sequence(es) -> 
        "[" +
            match es with 
            | [e] -> (to_string e) + "]"
            | e::es -> (to_string e) + ", " + (to_string (Sequence es))
            | [] -> ""

let simplify (expression: Expression) =
    // Apply a simplification, print the result, and repeat
    match expression with
    | Number(n) ->
        // Maximally simplified already
        expression
    | Variable(x) -> failwith "TODO"
    | Assignment(e1, e2) -> failwith "TODO"
    | Sequence(es) -> failwith "TODO"

let rec evaluate_expression (expression: Expression)(env: Environment) =
    printfn "Simplifying: %s" (to_string expression)
    failwith "TODO"

let rec evaluate (expression: Expression)(env: Environment) =
    match expression with
    | Sequence(es) -> 
        match es with 
        | [e] ->
            evaluate_expression e env
        | e::es -> 
            let e', env' = evaluate_expression e env
            evaluate (Sequence es) env'
        | [] ->
            printfn "Invalid Sequence. There must be at least one expression."
            exit 1
    | _ -> 
        printfn "Top layer expression must be a sequence."
        exit 1