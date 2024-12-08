// Macro to get running time
#time 
type equation = {
    testValue: int64
    numbers: int64 list
}

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> List.ofArray
    |> List.map (fun s ->
        let row = s.Split(":", System.StringSplitOptions.RemoveEmptyEntries)
        {
            testValue = row.[0] |> int64
            numbers = row.[1].Split(" ", System.StringSplitOptions.RemoveEmptyEntries) 
                |> List.ofArray 
                |> List.map (fun s -> s |> int64)
        })

let sumOperator (total: int64) (n: int64) = total+n
let mulOperator (total: int64) (n: int64) = total*n
let concatenateOperator (total: int64) (n: int64) = (total |> string) + (n |> string) |> int64 

let solveRowEquation (operators) (e: equation)  =

    let rec solve (total: int64) (numbersToSolve: int64 list) = 
        if numbersToSolve.Length = 0 
        then total = e.testValue
        elif total > e.testValue 
        then false
        else
            let n = numbersToSolve |> List.head
            let tail = numbersToSolve |> List.tail
            operators 
            |> List.map (fun o -> solve (o total n) tail)
            |> List.exists (fun f -> f = true)
    let recResult = solve 0 e.numbers
    (recResult, e.testValue)

let calibration operators (eList: equation list) =
    eList
    |> List.map (fun e -> solveRowEquation operators e)
    |> List.filter fst 
    |> List.map snd
    |> List.sum

let exampleInput = parseInput "./input/day07_example.txt"
calibration [sumOperator;mulOperator] exampleInput
|> printfn "Example answer 1: %A"
calibration [sumOperator;mulOperator;concatenateOperator] exampleInput
|> printfn "Example answer 2: %A"

let input = parseInput "./input/day07.txt"
calibration [sumOperator;mulOperator]input
|> printfn "Answer 1: %A"
calibration [sumOperator;mulOperator;concatenateOperator]input
|> printfn "Answer 2: %A"