// Macro to get running time
#time 

// Newline
let nl = "\n"

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> Array.map (fun f -> 
        f.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> List.ofArray)
    |> List.ofArray

let levelIncreasePairwiser (report: int list) = 
    let levelPairs = 
        report 
        |> List.pairwise
        |> List.groupBy (fun (first, second) -> if first < second then true else false)
        |> List.map fst
    
    let trues = levelPairs |> List.where (fun f -> f = true)
    let falses = levelPairs |> List.where (fun f -> f = false)
    if trues > falses then report |> List.pairwise
    else report |> List.rev |> List.pairwise

let isPairSafe (first: int, second: int) = first < second && second - first <= 3

let safetyInspection (report: int list) =
    report
    |> levelIncreasePairwiser
    |> List.forall (isPairSafe)

let safetyInspectionBruteDamper (report: int list) =
    if safetyInspection report then true
    else 
        let bruteReports = 
            report |> List.mapi (fun i _ -> report |> List.removeAt i |> safetyInspection)
        bruteReports |> List.exists (fun f -> f = true)

let reportCalculator (reports: bool list) = reports |> List.where (fun f -> f = true) |> List.length

parseInput "./input/day02_example.txt" 
|> List.map safetyInspection
|> reportCalculator
|> printfn "Example answer1: %d"

parseInput "./input/day02_example.txt" 
|> List.map safetyInspectionBruteDamper
|> reportCalculator
|> printfn "Example answer2: %d"

parseInput "./input/day02.txt" 
|> List.map safetyInspection
|> reportCalculator
|> printfn "Answer1: %d"

parseInput "./input/day02.txt" 
|> List.map safetyInspectionBruteDamper
|> reportCalculator
|> printfn "Answer2: %d"
