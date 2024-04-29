open System.IO
open Parser
open Evaluator

[<EntryPoint>]
let main args =
    // Check for correct usage
    if args.Length <> 1 && args.Length <> 2 && args.Length <> 3 then
        printfn "Usage: dotnet run <program> [output filename] [-DEBUG]"
        exit 1
    
    let input_file = args[0]
    let input = File.ReadAllText input_file
    let debug_on = false

    let result = parse input debug_on
    match result with
    | Some ast -> printfn "%A" ast
    //| Some ast -> evaluate ast
    | None -> printfn "Invalid Program."
    0