// Macro to get running time
#time 

// Newline
let nl = "\n"

let parseInput (filePath)  =
    let tupleArr = 
        System.IO.File.ReadAllLines filePath
        |> Array.map (fun f -> 
            let sArr = f.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
            (sArr |> Array.head |> int64, sArr |> Array.last |> int64))
    let firstList = tupleArr |> Array.map fst |> List.ofArray
    let secondList = tupleArr |> Array.map snd |> List.ofArray
    (firstList, secondList)

let pairList (firstList, secondList) =
    let sortedFirstList = firstList |> List.sort
    let sortedSecondList = secondList |> List.sort
    sortedFirstList |> List.mapi (fun (i : int) (num : int64) -> abs(num - sortedSecondList.[i]))

let similarityScore (firstList: int64 list, secondList: int64 list) =
    firstList |> List.map(fun (num: int64) -> 
        let occurances = 
            secondList 
            |> List.where (fun num2 -> num2 = num) 
            |> List.length
            |> int64
        occurances * num 
    )

let exampleInput = parseInput "./input/day01_example.txt" 
exampleInput
|> pairList
|> List.sum
|> printfn "Example distance: %d"  
|> ignore

exampleInput
|> similarityScore
|> List.sum
|> printfn "Example similarity: %d"  
|> ignore


let input = parseInput "./input/day01.txt" 
input
|> pairList
|> List.sum
|> printfn "Answer1 distance: %d"  
|> ignore


input
|> similarityScore
|> List.sum
|> printfn "Answer2 similarity: %d"  
|> ignore
