open System.IO
open System.Diagnostics
open Parser
open Evaluator

[<EntryPoint>]
let main args =
    // Check for correct usage
    if args.Length <> 1 && args.Length <> 2 && args.Length <> 3 then
        printfn "Usage: dotnet run <program> [output filename] [-DEBUG]"
        exit 1
    
    // Ensure filename is in correct format
    if args[0].Length < 4 || args[0].Substring (args[0].Length - 4) <> ".cat" then
        printfn "Invalid filename. (Note: programs require a .cat extension)"
        exit 1

    // Read in the input file
    let input = 
        try
            File.ReadAllText args[0]
        with
            | _ -> 
                printfn "Error reading program."
                exit 1

    // Hardcoded debug flag, refactor to check input later
    let debug_on = false

    let result = parse input debug_on
    let _, latex =
        match result with
        | Some ast -> evaluate ast
        | None -> 
            printfn "Invalid Program."
            exit 1

    // Create the output latex file (this substring is safe because we checked earlier)
    let output_file = args[0].Substring (0, args[0].Length - 4) + ".tex"
    try
        File.WriteAllText (output_file, latex)
    with
        | _ -> 
            printfn "Error creating latex file."
            exit 1
    0