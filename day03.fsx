// Macro to get running time
#time 

open System.Text.RegularExpressions

let mulRegex = Regex(@"mul\(\d+\,\d+\)", RegexOptions.Compiled)

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> List.ofArray
    |> List.fold (fun stateS s -> 
        (stateS + s)
            .Replace("\r\n", "")
            .Replace("\n","") 
        ) ""

let multiplys (input: string) (regexToBeUsed: Regex) =
    let muls = 
        seq {
            for m in regexToBeUsed.Matches(input) do
                yield m.Value
        }
    muls
    |> List.ofSeq
    |> List.map (fun matchString -> 
        matchString
            .Substring(4)
            .Substring(0, (matchString.Length - 5))
            .Split(",")
            |> Array.map int
            |> Array.fold (fun stateInt int -> stateInt * int) 1) 
    |> List.sum

let exampleInput = parseInput "./input/day03_example.txt" 
multiplys exampleInput mulRegex
|> printfn "Example answer 1: %d"

let input = parseInput "./input/day03.txt" 
multiplys input mulRegex
|> printfn "Answer 1: %d"