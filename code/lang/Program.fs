open System.IO
open Parser
open Evaluator

[<EntryPoint>]
let main args =
    // Check for correct usage
    if args.Length <> 1 && args.Length <> 2 && args.Length <> 3 then
        printfn "Usage: dotnet run <program> [output filename] [-DEBUG]"
        exit 1
    
    // Reaed in the input file
    let input_file = args[0]
    let input = 
        try
            File.ReadAllText input_file
        with
            | _ -> 
                printfn "Invalid filename."
                exit 1

    // Hardcoded debug flag, refactor to check input later
    let debug_on = false

    let result = parse input debug_on
    match result with
    | Some ast -> evaluate ast |> ignore
    | None -> printfn "Invalid Program."
    0