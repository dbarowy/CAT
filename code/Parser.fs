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
        (pseq digits period concatenate) 
        digits 
        concatenate
    <|> digits) <!> "floating_point"
let number = 
    (pseq 
        (pstr "-")
        floating_point
        concatenate
    <|> floating_point) |>> (fun x -> Number (double x)) <!> "number"

let variable = plower |>> Variable <!> "variable"

let literals = number <|> variable

let parentheses = 
    pbetween 
        (pchar '(') 
        (precedence 3) 
        (pchar ')') 
    |>> Parentheses <!> "parentheses"

let multiplication = 
    pseq 
        (precedence 2) // Higher precedence on left prevents infinite recursion
        (pright (pad (pchar '*')) (precedence 2)) 
        Multiplication
            
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
                    | '-' -> Multiplication (Number -1, expr)
                    | _ -> 
                        printfn "Illegal addition or subtraction symbol."
                        exit 1
                )
            )
        )
        (fun (e, es) -> Addition (e::es))

expressionImpl := 
    pad (precedence 0) <!> "expression"


let rec precedenceRecImpl level =
    match level with
    | 0 -> additionOrSubtraction <|> precedence (level + 1)
    | 1 -> multiplication <|> precedence (level + 1)
    | 2 -> parentheses <|> precedence (level + 1)
    | 3 -> literals
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