module Simplifier

open AST

// Defines an order for expression to be sorted by
let rec exprSorter e1 e2 =
    let rec expr_to_ordinal expr =
        match expr with
        | Number _ -> 1
        | Variable _ -> 2
        | Addition _ -> 3
        | Multiplication _ -> 4
        | Exponentiation _ -> 5
        | Sequence _ -> failwith "Shouldn't sort a sequence."
    
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

// Rearranges terms to obey the standard ordering
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

let rec combine_like_terms expr_list combiner =
    // Combines as many terms as possible with the given term,
    // using the provided combiner
    // Returns a tuple of the combined term and the leftover terms
    let combine_list_with_term term terms combiner =
        let combined_term, other_terms =
            List.fold 
                (fun (partially_combined_term, leftover_terms) expr ->
                    match combiner partially_combined_term expr with
                    | Some expr -> expr, leftover_terms
                    | None -> partially_combined_term, expr::leftover_terms)
                (term, [])
                terms
        // Note that we reverse other terms because folding through the list
        // causes it to accumlate backwards, and we also 
        combined_term, List.rev other_terms

    // Iterate through the list and for each term, combine all subsequent terms
    // with it that are compatible
    match expr_list with
    | e::es ->
        // Combine all like terms with the first term
        let combined_term, leftover_terms = combine_list_with_term e es combiner

        // Try to combine any remaining terms with other remaining terms
        let other_terms = combine_like_terms leftover_terms combiner

        // Return the list of now combined terms
        combined_term::other_terms
    | [] -> []

// Tries to simplify the addition of two expressions into a single expression
let addition_term_combiner expand e1 e2 =
    match e1, e2 with
    | Number n1, Number n2 -> 
        Some (Number (n1 + n2))
    
    | e1, Number 0.0 -> Some e1
    | Number 0.0, e2 -> Some e2
    
    // Simple variable combining
    | e1, e2 when e1 = e2 -> 
        Some (reorder_terms (flatten_ast (Multiplication [Number 2; e1])))
    | e1, Multiplication (Number n::e2s) 
        when e1 = (if e2s.Length > 1 then Multiplication e2s else if e2s.Length <> 0 then List.head e2s else Multiplication []) -> 
        Some (Multiplication [Number (1.0 + n); e1])
    | Multiplication (Number n::e1s), e2
        when e2 = (if e1s.Length > 1 then Multiplication e1s else if e1s.Length <> 0 then List.head e1s else Multiplication []) -> 
        Some (Multiplication [Number (1.0 + n); e2])
    | Multiplication [Number n; Variable x1], Variable x2 when x1 = x2 -> 
        Some (Multiplication [Number (1.0 + n); Variable x1])
    
    // Technically also done by the factoring rules, but this is a more human shortcut
    // and will also be applied when we are expanding
    | Multiplication(Number n1::e1s), Multiplication(Number n2::e2s) when e1s = e2s -> 
        Some (Multiplication ((Number (n1 + n2))::e1s))
    | Multiplication e1s, Multiplication(Number n::e2s) when e1s = e2s -> 
        Some (Multiplication ((Number (1.0 + n))::e1s))
    | Multiplication (Number n::e1s), Multiplication e2s when e1s = e2s -> 
        Some (Multiplication ((Number (1.0 + n))::e1s))

    // Factoring rules (some are redundant but more efficient/look more natural)
    | Multiplication e1s, Multiplication (e2::e2s) when e1s = e2s && not expand -> 
        Some (Multiplication ((Addition [Number 1; e2])::e1s))
    | Multiplication (e1::e1s), Multiplication e2s when e1s = e2s && not expand -> 
        Some (Multiplication ((Addition [Number 1; e1])::e1s))
    | Multiplication(e1::e1s), Multiplication(e2::e2s) when e1 = e2 && not expand = false -> 
        Some (Multiplication [e1; Addition [Multiplication e1s; Multiplication e2s]]) 
    | Multiplication(e1::e1s), Multiplication(e2::e2s) when e1s = e2s && not expand = false -> 
        Some (Multiplication ((Addition [e1; e2])::e1s))
    | Multiplication e1s, Multiplication e2s 
        when not (Set.isEmpty (Set.intersect (Set.ofList e1s) (Set.ofList e2s))) 
        && not expand ->
        let intersection = Set.intersect (Set.ofList e1s) (Set.ofList e2s)
        let otherE1s = Set.toList (Set.difference (Set.ofList e1s) intersection)
        let otherE2s =  Set.toList (Set.difference (Set.ofList e2s) intersection)
        Some (reorder_terms (flatten_ast (Multiplication ((Addition [Multiplication otherE1s; Multiplication otherE2s])::(Set.toList intersection)))))
    | e1, Multiplication e2s
        when List.contains e1 e2s && not expand ->
        let other_terms = List.filter (fun e -> e <> e1) e2s
        let as_expression = 
            if other_terms.Length > 1 then Multiplication other_terms else List.head other_terms
        Some (reorder_terms (flatten_ast (Multiplication [e1; Addition [Number 1; as_expression]])))
    | Multiplication e1s, e2
        when List.contains e2 e1s && not expand ->
        let other_terms = List.filter (fun e -> e <> e2) e1s
        let as_expression = 
            if other_terms.Length > 1 then Multiplication other_terms else List.head other_terms
        Some (reorder_terms (flatten_ast (Multiplication [e2; Addition [Number 1; as_expression]])))
    | _ -> None

// Tries to simplify the multiplication of two expressions into a single expression
let multiplication_term_combiner expand e1 e2 =
    match e1, e2 with
    | Number n1, Number n2 -> 
        Some (Number (n1 * n2))
    | e1, e2 when e1 = e2 -> 
        Some (Exponentiation (e1, Number 2))
    | Exponentiation (e1, exponent1),
        Exponentiation (e2, exponent2) 
        when e1 = e2 && not expand -> 
        Some (Exponentiation (e1, Addition [exponent1; exponent2]))
    | Exponentiation (e1, exponent1),
        Exponentiation (e2, exponent2) 
        when exponent1 = exponent2 && not expand -> 
        Some (reorder_terms (flatten_ast (Exponentiation (Multiplication [e1; e2], exponent1))))
    | e1, Exponentiation (e2, exponent) 
        when e1 = e2 && not expand -> 
        Some (Exponentiation (e1, Addition [exponent; Number 1]))
    | Exponentiation (e1, exponent), e2 
        when e1 = e2 && not expand -> 
        Some (Exponentiation (e1, Addition [exponent; Number 1]))
    | _ -> None

(*
 * @return A list of progressively more simplified versions of the expression
 *)
let rec simplify (expression: Expression)(expand: bool) =
    // Preprocess expression, so it's in the expected form
    let expression = reorder_terms (flatten_ast expression)

    match expression with
    | Number _ | Variable _ ->
        [] // Maximally simplified already
    | Exponentiation(exponent_base, exponent) ->
        simplify_exponentiation exponent_base exponent expand
    | Multiplication(es) -> 
        simplify_multiplication es expand
    | Addition(es) -> 
        simplify_addition es expand
    | Sequence(es) -> 
        failwith "Sequence should not be passed to simplify."

// Returns a list of progressive simplifications of a list of expressions 
// The list of expressions are combined by the combining_type operation into 
// a single expression (for example Addition or Multiplication)
and simplify_list_of_terms terms combining_type already_simplified_terms expand =
    match terms with
    | e::es ->
        // Get the list of simplifications for this expression within the larger list
        let simplifications = simplify e expand

        // If there weren't any changes, then disregard this element
        if simplifications.Length = 0 then
            (simplify_list_of_terms es combining_type (already_simplified_terms @ [e]) expand) // Get rid of list concatenations
        else
            // Otherwise, recreate the overall expression for each simplification step
            (List.map (fun e -> reorder_terms (flatten_ast (combining_type (already_simplified_terms @ e::es)))) simplifications) 
                @ (simplify_list_of_terms es combining_type (already_simplified_terms @ [List.last simplifications]) expand)
    | [] -> []

and simplify_addition terms expand =
    printfn "Terms: %A, expand: %A" terms expand
    // Combine like terms in the sum
    let combined_terms = combine_like_terms terms (addition_term_combiner expand)
    printfn "Combined Terms: %A" combined_terms
    if terms <> combined_terms then
        // Check if we reduced it to a single term
        match combined_terms with
        | [expr] -> expr::(simplify expr expand)
        | exprs -> 
            let expr = reorder_terms (Addition exprs)
            expr::(simplify expr expand)
    else

    if expand then
        let expanded_terms_simplifications = simplify_list_of_terms terms Addition [] true
        
        // Once we've expanded we no longer need to
        if expanded_terms_simplifications.Length <> 0 then
            expanded_terms_simplifications @ (simplify (List.last expanded_terms_simplifications) false)
        else
            simplify (Addition terms) false
    else

    // Try to simplify each term individually
    let simplifications: Expression list = simplify_list_of_terms terms Addition [] expand
    if simplifications.Length <> 0 then
        simplifications @ (simplify (List.last simplifications) expand)
    else

    // No other simplifications to try
    []

and simplify_multiplication terms expand =
    let combined_terms = combine_like_terms terms (multiplication_term_combiner expand)
    if terms <> combined_terms then
        // Check if we reduced it to a single term
        match combined_terms with
        | [expr] -> expr::(simplify expr expand)
        | exprs -> 
            let expr = reorder_terms (Multiplication exprs)
            expr::(simplify expr expand)
    else

    if expand then
        let expanded_terms_simplifications = simplify_list_of_terms terms Multiplication [] true
        
        // Insert distributing here <----------------------------------------

        // Once we've expanded we no longer need to
        if expanded_terms_simplifications.Length <> 0 then
            expanded_terms_simplifications @ (simplify (List.last expanded_terms_simplifications) false)
        else
            simplify (Multiplication terms) false
    else

    // Try to simplify each term individually
    let simplifications: Expression list = simplify_list_of_terms terms Multiplication [] expand
    if simplifications.Length <> 0 then
        simplifications @ (simplify (List.last simplifications) expand)
    else

    // Check if we multiply by 0
    if List.exists (fun e -> e = Number 0) terms then
        [Number 0]
    else

    // No other simplifications to try
    []

and simplify_exponentiation exponent_base exponent expand =
    // First simplify the base
    let base_simplifications = simplify exponent_base expand
    let combined_base_simplifications =
        if base_simplifications.Length = 0 then
            []
        else
            List.map (fun e -> Exponentiation(e, exponent)) base_simplifications
    // Track which base to use in the exponent simplifcations
    let final_base_version = 
        if base_simplifications.Length > 0 then List.last base_simplifications else exponent_base
    
    // Simplify the exponent
    let exponent_simplifications = simplify exponent expand
    let combined_exponent_simplifications = 
        if exponent_simplifications.Length = 0 then
                []
        else
            List.map (fun e -> Exponentiation(final_base_version, e)) exponent_simplifications
    
    let final_exponent_version =
        if exponent_simplifications.Length > 0 then List.last exponent_simplifications else exponent

    let combined_expr =
        match final_base_version, final_exponent_version with
        | Number b, Number e ->
            [Number (b ** e)]
        | b, Addition es when expand ->
            let split_version = Multiplication (List.map (fun e -> Exponentiation (b, e)) es)
            split_version::(simplify split_version true)
        | Multiplication bs, e when expand ->
            let distributed_version = Multiplication (List.map (fun b -> Exponentiation (b, e)) bs)
            distributed_version::(simplify distributed_version true)

        | _ -> []
    
    // Insert expanding here

    // Combine the series of simplifications
    combined_base_simplifications @ combined_exponent_simplifications @ combined_expr // Should there be a recursive simplify call here?