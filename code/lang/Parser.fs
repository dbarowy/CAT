module Parser

open Combinator
open AST

let expression, expressionImpl = recparser()
let precedence, precedenceImpl = recparser1()

let pad parser = pbetween pwsNoNL0 parser pwsNoNL0
let period = pstr "." <!> "period"
let digits = pmany1 pdigit |>> stringify <!> "digits"
let concatenate (a, b) = a + b
let floating_point = 
    (pseq 
        (pseq digits period concatenate <|> period) 
        digits 
        concatenate
    <|> pseq 
            digits
            period
            concatenate
    <|> digits) <!> "floating_point"
    
let number = floating_point |>> (fun x -> Number (double x)) <!> "number"

let variable = plower |>> Variable <!> "variable"

let literals = number <|> variable

let parentheses = 
    pbetween 
        (pchar '(') 
        (precedence 0) 
        (pchar ')') 
    <!> "parentheses"

let rec unfold_exponentiation es =
    match es with
    | [e] -> e
    | e::es' -> Exponentiation (e, (unfold_exponentiation es'))
    | [] -> failwith "Don't pass unfold_exponentiation an empty list."

let exponentiation = 
    pseq 
        (precedence 3) 
        (pmany1 
            (pright 
                (pad (pchar '^')) 
                (precedence 3)
            ) 
        )
        (fun (e1, es) -> unfold_exponentiation (e1::es))
    <!> "Exponentiation"
let multiplicationOrDivision = 
    pseq 
        (precedence 2) 
        (pmany1 
            (pseq 
                ((pad (pchar '*')) <|> (pad (pchar '/'))) 
                (precedence 2)
                (fun (symbol, expr) -> 
                    match symbol with
                    | '*' -> expr
                    | '/' -> Exponentiation (expr, Number -1)
                    | _ -> 
                        printfn "Illegal multiplication or division symbol."
                        exit 1
                )
            <|> pad (precedence 2)
            ) 
        )
        (fun (e, es) -> Multiplication (e::es))
    <|> pseq
        (pad (pchar '-'))
        (precedence 1)
        (fun (_, expr) -> Multiplication [Number -1; expr])
    <!> "multiplicationOrDivision"
         
let additionOrSubtraction = 
    pseq 
        (precedence 1) 
        (pmany1 
            (pseq 
                ((pad (pchar '+')) <|> (pad (pchar '-'))) 
                (precedence 1)
                (fun (symbol, expr) -> 
                    match symbol with
                    | '+' -> expr
                    | '-' -> Multiplication [Number -1; expr]
                    | _ -> 
                        printfn "Illegal addition or subtraction symbol."
                        exit 1
                )
            )
        )
        (fun (e, es) -> Addition (e::es))
    <!> "additionOrSubtraction"

expressionImpl := 
    pad (precedence 0) <!> "expression"

let rec precedenceRecImpl level =
    match level with
    | 0 -> pad (additionOrSubtraction <|> precedence (level + 1) <!> "precedence 0")
    | 1 -> pad (multiplicationOrDivision <|> precedence (level + 1) <!> "precedence 1")
    | 2 -> pad (exponentiation <|> precedence (level + 1) <!> "precedence 2")
    | 3 -> pad (parentheses <|> precedence (level + 1) <!> "precedence 3")
    | 4 -> pad (literals <!> "precedence 4")
    | _ -> failwith "Illegal Precedence Level."
precedenceImpl := precedenceRecImpl


let instruction_seq = 
    pseq 
        expression
        (pmany0 (pright pnl expression)) 
        (fun (e, es) -> e::es) 
    |>> Sequence <!> "expr_seq"
let grammar = pleft instruction_seq peof <!> "grammar"

let parse (input: string)(debug_on: bool) : Expression option =
    match grammar (if debug_on then debug input else prepare input) with
    | Success(ast, _) -> Some ast
    | Failure(fail_pos, rule) -> 
        printfn "Error at pos: %d while parsing rule: %s" fail_pos rule
        printfn "%s" (input[fail_pos-10..fail_pos-1] + "----->" + input[fail_pos..fail_pos+10])
        None