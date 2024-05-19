module Evaluator

open AST
open Simplifier
open System.IO

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
    | Exponentiation _ -> false //(Stylistic difference)
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
        | (Exponentiation (b1,e1))::(Exponentiation (b2,e2))::es ->
            "(" + to_string (Exponentiation (b1, e1)) + ")" 
                + to_string (Multiplication ([Exponentiation (b2, e2)] @ es))
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

// Returns whether or not this type of expression should be parenthesized
// if it is part of a more complicated expression
let needs_parens_latex expr =
    match expr with
    | Number n when n >= 0 -> false
    | Variable _ -> false
    | Exponentiation _ -> false // (Stylistic difference)
    | _ -> true 

let rec to_latex expression =
    match expression with
    | Number(n) -> string n
    | Variable(x) -> string x
    | Addition(es) -> 
        match es with
        | [e] -> to_latex e
        | e::es -> (to_latex e) + " + " + (to_latex (Addition es))
        | [] -> failwith "Invalid Addition"
    | Multiplication(es) -> 
        // Check whether or not we can write it as an implicit multiplication
        match es with
        | e::es -> 
            let s = if needs_parens e then "(" + to_latex e + ")" else to_latex e
            let tail = to_latex (Multiplication es)
            if (ends_with floating_point_char s && begins_with floating_point_char tail) then
                "(" + s + ")" + to_latex (Multiplication es)
            else
                s + to_latex (Multiplication es)
        | [] -> ""
    | Exponentiation(e1, e2) -> 
        // Check if we need to wrap either part in parentheses
        match needs_parens e1 with
        | true ->
            "(" + (to_latex e1) + ")^{" + (to_latex e2) + "}"
        | false ->
            (to_latex e1) + "^{" + (to_latex e2) + "}"
            
    | Sequence(es) -> // Unused
        "\\begin{document}\n" +
        match es with 
        | e::es -> "$$" + (to_latex e) + "$$\n" + (to_latex (Sequence es))
        | [] -> "\\end{document}"

let process_expression (expression: Expression) =
    printfn "Expanding: %s" (to_string expression)
    let expansions = (reorder_terms (flatten_ast expression))::(expand expression)

    // Print out each subsequent expansion and return the final version
    let expanded_expression, latex =
        List.fold
            (fun (_, latex) e -> 
                printfn "==> %s" (to_string e)
                e, latex + "$$" + to_latex e + "$$\n")
            (expression, "") // these are dummy placeholders
            expansions
    
    printfn "Simplifying: %s" (to_string expanded_expression)
    let simplifications = (reorder_terms (flatten_ast expanded_expression))::(simplify expanded_expression)

    // Print out each subsequent expansion and return the final version
    List.fold
        (fun (_, latex) e -> 
            printfn "==> %s" (to_string e)
            e, latex + "$$" + to_latex e + "$$\n")
        (expression, latex) // Expression is a dummy placeholder
        simplifications

let LATEX_HEADER = 
    "\\documentclass[10pt]{article}\n" +
    "\\usepackage[left=1in,top=1in,right=1in,bottom=1in]{geometry}\n" +
    "\\begin{document}\n"
let LATEX_FOOTER = "\\end{document}"

let rec evaluate (expression: Expression) =
    let rec rec_evaluator expression = 
        match expression with
        | Sequence(es) -> 
            match es with 
            | [e] ->
                let expr, latex = process_expression e
                [expr], "Simplifying: $" + to_latex e + "$\n" + latex
            | e::es -> 
                let expr, latex = process_expression e
                printfn ""
                let other_exprs, other_latex = rec_evaluator (Sequence es)
                expr::other_exprs, "Simplifying: $" + to_latex e + "$\n" + latex + other_latex
            | [] -> failwith "Invalid Sequence. There must be at least one expression."
        | _ -> failwith "Top layer expression must be a sequence."
    let final_expression, latex = rec_evaluator expression
    final_expression, LATEX_HEADER + latex + LATEX_FOOTER