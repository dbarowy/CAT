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

let rec exprSorter e1 e2 =
    let rec expr_to_ordinal expr =
        match expr with
        | Number _ -> 1
        | Variable _ -> 2
        | Exponentiation _ -> 3
        | Addition _ -> 4
        | Multiplication _ -> 5
        | Sequence _ -> failwith "Can't order a sequence."
    
    let e1_ordinal = expr_to_ordinal e1
    let e2_ordinal = expr_to_ordinal e2
    if e1_ordinal < e2_ordinal then
        -1
    else if e1_ordinal > e2_ordinal then
        1
    else
        match e1, e2 with 
        | Number n1, Number n2 -> int(n1 - n2)
        | Variable x1, Variable x2 -> int(x1) - int(x2)
        | Exponentiation (b1, e1), Exponentiation(b2, e2) -> 
            let result = exprSorter b1 b2
            if result = 0 then
                exprSorter e1 e2
            else 
                result
        | Addition e1s, Addition e2s -> 
            match e1s, e2s with
            | e1'::e1s', e2'::e2s' ->
                let result = exprSorter e1' e2'
                if result = 0 then
                    exprSorter (Addition e1s') (Addition e2s')
                else 
                    result
            | [], [] -> 0
            | _, [] -> 1
            | [], _ -> -1
        | Multiplication e1s, Multiplication e2s -> 
            match List.rev e1s, List.rev e2s with
            | e1'::e1s', e2'::e2s' ->
                let result = exprSorter e1' e2'
                if result = 0 then
                    exprSorter (Multiplication e1s') (Multiplication e2s')
                else 
                    result
            | [], [] -> 0
            | _, [] -> 1
            | [], _ -> -1
        | _ -> failwith "Impossible Case."

let rec reorder_terms expr =
    match expr with
    | Number _ | Variable _ -> expr
    | Exponentiation (e1, e2) -> Exponentiation (reorder_terms e1, reorder_terms e2)
    | Sequence es -> failwith "Reording doesn't apply to sequences."
    | Addition es ->
        let terms = List.map reorder_terms es
        Addition(List.sortWith exprSorter terms)
    | Multiplication es ->
        let terms = List.map reorder_terms es
        Multiplication(List.sortWith exprSorter terms)

// Converts associative operations that are unnecessarily nested into a single list
let rec flatten_ast expr =
    match expr with
    | Number _ | Variable _ -> expr
    | Exponentiation(e1, e2) -> Exponentiation(flatten_ast e1, flatten_ast e2)
    | Sequence(es) ->
        Sequence
            (List.fold
                (fun flattened_es expr ->
                    (flatten_ast expr)::flattened_es)
                []
                es)
    | Addition(es) when es.Length = 1 -> flatten_ast (List.head es)
    | Addition(es) -> 
        Addition 
            (List.fold 
                (fun flattened_es expr ->
                    let flattened_expr = flatten_ast expr
                    match flattened_expr with 
                    | Addition(nested_es) -> 
                        nested_es @ flattened_es
                    | _ -> flattened_expr::flattened_es)
                []
                es)
    | Multiplication(es) when es.Length = 1 -> flatten_ast (List.head es)
    | Multiplication(es) -> 
        Multiplication 
            (List.fold 
                (fun flattened_es expr ->
                    let flattened_expr = flatten_ast expr
                    match flattened_expr with 
                    | Multiplication(nested_es) -> 
                        nested_es @ flattened_es
                    | _ -> flattened_expr::flattened_es)
                []
                es)

let rec add_like_terms expr_list =
    let are_like_terms e1 e2 =
        match e1, e2 with
        | Number _, Number _ -> true
        | Variable x1, Variable x2 when x1 = x2 -> true
        | _ -> false
    
    let combine e1 e2 =
        match e1, e2 with
        | Number n1, Number n2 -> Number (n1 + n2)
        | Variable x1, Variable x2 -> Multiplication [Number 2; Variable x1]
        | _ -> failwith "Tried to combine unlike terms."

    let combine_with_term term terms =
        List.fold 
            (fun (combined_term, remaining_terms, succeeded) expr ->
                if are_like_terms combined_term expr then
                    combine combined_term expr, remaining_terms, true
                else
                    combined_term, expr::remaining_terms, succeeded)
            (term, [], false)
            terms
    
    match expr_list with
    | e::es ->
        let e', es', s1 = combine_with_term e es
        let other_terms, s2 = add_like_terms es'
        e'::other_terms, s1 || s2
    | [] -> [], false

let rec multiply_like_terms expr_list =
    let are_like_terms e1 e2 =
        match e1, e2 with
        | Number _, Number _ -> true
        | Variable x1, Variable x2 when x1 = x2 -> true
        | e1, Exponentiation (e2, _) when e1 = e2 -> true
        | Exponentiation (e1, _), e2 when e1 = e2 -> true
        | Exponentiation (e1, _), Exponentiation(e2, _) when e1 = e2 -> true
        | _ -> false
    
    let combine e1 e2 =
        match e1, e2 with
        | Number n1, Number n2 -> Number (n1 * n2)
        | Variable x1, Variable x2 -> Exponentiation (Variable x1, Number 2)
        | Exponentiation (e1, exponent1), Exponentiation (e2, exponent2) -> 
            Exponentiation (e1, Addition [exponent1; exponent2])
        | e1, Exponentiation (e2, exponent) -> Exponentiation (e1, Addition [exponent; Number 1])
        | Exponentiation (e1, exponent), e2 -> Exponentiation (e1, Addition [exponent; Number 1])
        | _ -> failwith "Tried to combine unlike terms."

    let combine_with_term term terms =
        List.fold 
            (fun (combined_term, remaining_terms, succeeded) expr ->
                if are_like_terms combined_term expr then
                    combine combined_term expr, remaining_terms, true
                else
                    combined_term, expr::remaining_terms, succeeded)
            (term, [], false)
            terms
    
    match expr_list with
    | e::es ->
        let e', es', s1 = combine_with_term e es
        let other_terms, s2 = multiply_like_terms es'
        e'::other_terms, s1 || s2
    | [] -> [], false

(*
 * @return A list of progressively more simplified versions of the expression
 *)
let rec simplify (expression: Expression) =
    let expression = reorder_terms (flatten_ast expression)

    match expression with
    | Number _ | Variable _ ->
        // Maximally simplified already
        []
    | Addition(es) -> 
        let combined_terms, succeeded = add_like_terms es
        if succeeded then
            let expression' = Addition(combined_terms)
            expression'::(simplify expression')
        else
            let simplification_list: Expression list = simplify_list es Addition []
            if simplification_list.Length <> 0 then
                simplification_list @ (simplify (List.last simplification_list))
            else
                []
    | Multiplication(es) -> 
        let combined_terms, succeeded = multiply_like_terms es
        if succeeded then
            let expression' = Multiplication(combined_terms)
            expression'::(simplify expression')
        else
            let simplification_list = simplify_list es Multiplication []
            if simplification_list.Length <> 0 then
                simplification_list @ (simplify (List.last simplification_list))
            else
                []
    | Exponentiation(expbase, exponent) -> 
        let base_simplifications = simplify expbase
        if base_simplifications.Length = 0 then
            
        else
            List.map (fun e -> Exponentiation(e, exponent)) // WIP <----------------------------------------------------------------------
    | Sequence(es) -> failwith "Sequence should not be passed to simplify."

// Returns a list of progressive simplifications of a list of expressions that are combined by the expr_type operation
and simplify_list expr_list expr_type prev =
    match expr_list with
    | e::es ->
        // Get the list of simplifications for this expression within the larger list
        let simplifications = simplify e

        // If there weren't any changes, then disregard this element
        if simplifications.Length = 0 then
            (simplify_list es expr_type (prev @ [e]))
        else
            // Otherwise, recreate the overall expression for each simplification step
            (List.map (fun e -> expr_type (prev @ e::es)) simplifications) 
                @ (simplify_list es expr_type (prev @ [List.last simplifications]))
    | [] -> []

let process_expression (expression: Expression) =
    printfn "Simplifying: %s" (to_string expression)
    let expressions = expression::(simplify expression)
    printfn "%A" (reorder_terms (flatten_ast (List.last expressions)))
    
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