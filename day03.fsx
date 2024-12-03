// Macro to get running time
#time 

open System.Text.RegularExpressions

type Instruction =
| Enable
| Disable

let mulRegex = Regex(@"mul\(\d+\,\d+\)", RegexOptions.Compiled)
let disableRegex = Regex(@"don't\(\)", RegexOptions.Compiled)
let enableRegex = Regex(@"do\(\)", RegexOptions.Compiled)

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> List.ofArray
    |> List.fold (fun stateS s -> 
        (stateS + s)
            .Replace("\r\n", "")
            .Replace("\n","") 
        ) ""

let matchRegex (s: string) (r: Regex) =
    seq {
        for m in r.Matches(s) do
            yield m.Value, m.Index
    }
    |> List.ofSeq

let calculateMultiply (mulString: string) =
    mulString
        .Substring(4)
        .Substring(0, (mulString.Length - 5))
        .Split(",")
    |> Array.map int
    |> Array.fold (fun stateInt int -> stateInt * int) 1

let multiplys (input: string)  =
    matchRegex input mulRegex
    |> List.map (fun (matchString,_) -> calculateMultiply matchString)
    |> List.sum

let disableMultiplys (input: string) =
    let disables = matchRegex input disableRegex |> List.map (fun (_, i) -> (Disable, i))
    let enabled = matchRegex input enableRegex |> List.map (fun (_, i) -> (Enable, i))
    let instructions = enabled @ disables |> List.sortBy snd
        
    let multiplys = matchRegex input mulRegex
    multiplys
    |> List.fold (fun ((mState: string list), ((lastInsruction, lastInstructionIndex), dState: (Instruction * int) list)) (currentString, currentIndex) -> 
        let nextIndex = 
            match dState.Length with
            | 0 -> 2147483647
            | _ -> dState |> List.head |> snd
        let ((currentInsruction, currentInstructionIndex), newState) = 
            if lastInstructionIndex < currentIndex && nextIndex > currentIndex
            then ((lastInsruction, lastInstructionIndex), dState)
            else 
                ((dState |> List.head), dState |> List.tail)
        match currentInsruction with 
            | Enable -> (mState @ [currentString], ((currentInsruction, currentInstructionIndex), newState))
            | Disable -> (mState, ((currentInsruction, currentInstructionIndex), newState))
    ) ([], ((Enable, -1), instructions))
    |> fst
    |> List.map calculateMultiply
    |> List.sum 

let exampleInput = parseInput "./input/day03_example.txt" 
multiplys exampleInput
|> printfn "Example answer 1: %d"

let input = parseInput "./input/day03.txt" 
multiplys input
|> printfn "Answer 1: %d"

let example2Input = parseInput "./input/day03_example2.txt" 
example2Input
|> disableMultiplys
|> printfn "Example answer 2: %d"

input
|> disableMultiplys
|> printfn "Answer 2: %d"