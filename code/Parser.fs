module Parser

open Combinator
open AST

let expression, expressionImpl = recparser()

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

let parentheses = pbetween (pchar '(') expression (pchar ')') <!> "parentheses"
expressionImpl := 
    pbetween 
        pwsNoNL0
        (parentheses <|> number <|> variable)
        pwsNoNL0
    <!> "expression"

let assignment = 
    pbetween 
        pwsNoNL0 
        (pseq 
            (pleft variable pwsNoNL0) 
            (pright (pchar '=') expression) 
            Assignment) 
        pwsNoNL0
    <!> "assignment"

let instruction_seq = 
    pseq 
        (assignment <|> expression) 
        (pmany0 (pright pnl (assignment <|> expression))) 
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