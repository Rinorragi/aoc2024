// Macro to get running time
#time 

type cinate = {
    x: int
    y: int
}

type Match = 
| North
| NorthEast
| East
| SouthEast
| South
| SouthWest
| West
| NorthWest
| None

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> List.ofArray
    |> List.map (fun  s -> 
        s.Replace("\r\n","")
            .Replace("\n","")
            .ToLower()
            .ToCharArray()
        |> List.ofArray)

let findStartOfXmas (wordPuzzle: char list list) charToFind =
    wordPuzzle
    |> List.mapi(fun i charList -> 
        charList
        |> List.indexed
        |> List.filter (fun (ic, c) -> c = charToFind )
        |> List.map fst
        |> List.map (fun ic -> { x = ic; y = i}))
    |> List.concat

let findXmas (wordPuzzle: char list list) =
    let maxX = wordPuzzle.[0].Length
    let maxY = wordPuzzle.Length
    let wordLength = 3
    findStartOfXmas wordPuzzle 'x'
    |> List.map(fun c -> 
        [
        if c.x < maxX - wordLength && wordPuzzle.[c.y].[c.x+1] = 'm' && wordPuzzle.[c.y].[c.x+2] = 'a' && wordPuzzle.[c.y].[c.x+3] = 's' then East else None;
        if c.x > wordLength - 1 && wordPuzzle.[c.y].[c.x-1] = 'm' && wordPuzzle.[c.y].[c.x-2] = 'a' && wordPuzzle.[c.y].[c.x-3] = 's' then West else None;
        if c.y < maxY - wordLength && wordPuzzle.[c.y+1].[c.x] = 'm' && wordPuzzle.[c.y+2].[c.x] = 'a' && wordPuzzle.[c.y+3].[c.x] = 's' then South else None;
        if c.y > wordLength - 1 && wordPuzzle.[c.y-1].[c.x] = 'm' && wordPuzzle.[c.y-2].[c.x] = 'a' && wordPuzzle.[c.y-3].[c.x] = 's' then North else None;
        if c.x < maxX - wordLength && c.y > wordLength - 1 && wordPuzzle.[c.y-1].[c.x+1] = 'm' && wordPuzzle.[c.y-2].[c.x+2] = 'a' && wordPuzzle.[c.y-3].[c.x+3] = 's' then NorthEast else None;
        if c.x < maxX - wordLength && c.y < maxY - wordLength && wordPuzzle.[c.y+1].[c.x+1] = 'm' && wordPuzzle.[c.y+2].[c.x+2] = 'a' && wordPuzzle.[c.y+3].[c.x+3] = 's' then SouthEast else None;
        if c.x > wordLength - 1 && c.y < maxY - wordLength && wordPuzzle.[c.y+1].[c.x-1] = 'm' && wordPuzzle.[c.y+2].[c.x-2] = 'a' && wordPuzzle.[c.y+3].[c.x-3] = 's' then SouthWest else None;
        if c.x > wordLength - 1 && c.y > wordLength - 1 && wordPuzzle.[c.y-1].[c.x-1] = 'm' && wordPuzzle.[c.y-2].[c.x-2] = 'a' && wordPuzzle.[c.y-3].[c.x-3] = 's' then NorthWest else None])
    |> List.concat
    |> List.filter (fun m -> m <> None)
    |> List.length

let findDoubleMas (wordPuzzle: char list list) =
    let maxX = wordPuzzle.[0].Length
    let maxY = wordPuzzle.Length
    findStartOfXmas wordPuzzle 'a'
    |> List.filter (fun c -> not(c.x = 0 || c.y = 0 || c.x = maxX - 1 || c.y = maxY - 1))
    |> List.map (fun c -> 
        ([wordPuzzle.[c.y-1].[c.x-1];wordPuzzle.[c.y+1].[c.x+1]] |> List.sort,[wordPuzzle.[c.y-1].[c.x+1];wordPuzzle.[c.y+1].[c.x-1]] |> List.sort))
    |> List.filter (fun (firstList, secondList) -> firstList.[0] = 'm' && firstList.[1] = 's' && secondList.[0] = 'm' && secondList.[1] = 's')
    |> List.length

let exampleInput = parseInput "./input/day04_example.txt" 
findXmas exampleInput
|> printfn "Example answer 1: %d"

let input = parseInput "./input/day04.txt" 
findXmas input
|> printfn "Answer 1: %d"

findDoubleMas exampleInput
|> printfn "Example answer 2: %d"

findDoubleMas input
|> printfn "Answer 1: %d"