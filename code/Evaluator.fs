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

(*
 * @return A list of progressively more simplified versions of the expression
 *)
let rec simplify (expression: Expression)(env: Environment) =
    match expression with
    | Number(n) ->
        // Maximally simplified already
        [expression]
    | Variable(x) -> 
        if Map.containsKey x env then
            expression::(simplify env[x] env)
        else
            [expression]
    | Assignment(_, e) -> 
        // Simplifying Assignments will delete derivative information
        // --> This should perform all simplifications except substitution
        // figure out how to handle this recursively
    | Sequence(es) -> 
        printfn "Sequence should not be passed to simplify."
        exit 1

let process_expression (expression: Expression)(env: Environment) =
    printfn "Simplifying: %s" (to_string expression)
    let expressions = simplify expression env
    
    let expression', env' = 
        match expression with
        | Assignment(e1, _) ->
            match e1 with
            | Variable(x) ->
                List.fold
                    (fun (_: Expression, environment: Environment)(e: Expression) -> 
                        printfn "==> %c = %s" x (to_string e)
                        e, environment.Add (x, e))
                    (expression, env)
                    expressions
            | _ -> 
                printfn "Invalid Assignment. Left side of an assignment must be a variable."
                exit 1
        | _ -> 
            List.fold
                (fun _ e -> 
                    printfn "==> %s" (to_string e))
                ()
                expressions
            expression, env
    expression', env'

let rec evaluate (expression: Expression)(env: Environment) =
    match expression with
    | Sequence(es) -> 
        match es with 
        | [e] ->
            process_expression e env
        | e::es -> 
            let e', env' = process_expression e env
            printfn ""
            evaluate (Sequence es) env'
        | [] ->
            printfn "Invalid Sequence. There must be at least one expression."
            exit 1
    | _ -> 
        printfn "Top layer expression must be a sequence."
        exit 1