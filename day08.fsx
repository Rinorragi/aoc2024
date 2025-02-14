// Macro to get running time
#time 
type PositionType = | Antenna | Land | Antinode
type Position = {
    x: int
    y: int
    c: char
    posType: PositionType
}

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
    |> List.ofArray
    |> List.mapi (fun cy cArray ->
        cArray |> List.mapi (fun cx cc -> 
        {
            y = cy 
            x = cx 
            c = cc
            posType = 
                match cc with
                | '.' -> Land
                | _ -> Antenna
        }))

let antennaPositions (inputData: Position list list) =
    inputData 
    |> List.concat 
    |> List.filter (fun p -> p.posType = Antenna) 
    |> List.groupBy (fun p -> p.c)

let antinodeForPosition (p : Position) (p2: Position) (xMax: int) (yMax: int) (times: int) =
    let xSingleDiff = abs(p.x - p2.x)
    let ySingleDiff = abs(p.y - p2.y)
    let yMaxTimes = if ySingleDiff = 0 then 1 else yMax / ySingleDiff
    let xMaxTimes = if xSingleDiff = 0 then 1 else xMax / xSingleDiff
    let maxTimes = if yMaxTimes > times || xMaxTimes > times then times else [yMaxTimes;xMaxTimes] |> List.max
    [0..maxTimes]
    |> List.map (fun i -> 
        if i = 0 then []
        else 
            let yMultipleDiff = i * ySingleDiff
            let xMultipleDiff = i * xSingleDiff
            if (p.x >= p2.x && p.y >= p2.y) || (p2.x >= p.x && p2.y >= p.y)
            then 
                // Southeast 
                let ySE = ([p.y;p2.y] |> List.max) + yMultipleDiff
                let xSE = ([p.x;p2.x] |> List.max) + xMultipleDiff
                // Northwest
                let yNW = ([p.y;p2.y] |> List.min) - yMultipleDiff
                let xNW = ([p.x;p2.x] |> List.min) - xMultipleDiff
                [(xSE, ySE);(xNW, yNW)]
            else 
                // Northeast
                let yNE = ([p.y;p2.y] |> List.min) - yMultipleDiff
                let xNE = ([p.x;p2.x] |> List.max) + xMultipleDiff
                // Southwest
                let ySW = ([p.y;p2.y] |> List.max) + yMultipleDiff
                let xSW = ([p.x;p2.x] |> List.min) - xMultipleDiff
                [(xNE, yNE);(xSW, ySW)])
    |> List.concat


let antinodesForPositionList (xMax: int) (yMax: int) (maxTimes: int) (posList: Position list)  =
    posList
    |> List.mapi (fun i p -> 
        posList 
        |> List.removeAt i 
        |> List.map (fun p2 -> antinodeForPosition p p2 xMax yMax maxTimes))
    |> List.concat
    |> List.concat
    |> List.filter (fun (x,y) -> x >= 0 && x <= xMax && y >= 0 && y <= yMax)

let solveAntinodes (maxTimes: int) (antennas: (char * Position list) list) (inputData: Position list list) = 
    let xMax = inputData.[0].Length - 1
    let yMax = inputData.Length - 1
    antennas
    |> List.map (fun cPos -> snd cPos |> antinodesForPositionList xMax yMax maxTimes)
    |> List.concat
    |> List.distinct

let exampleInput = parseInput "./input/day08_example.txt"
let exampleAntennas = antennaPositions exampleInput
let exampleAntinodes = solveAntinodes 1 exampleAntennas exampleInput
//printMap exampleInput exampleAntinodes
exampleAntinodes
|> List.length
|> printfn "Example answer 1: %d"

let calculateAntinodePart2 (antennas: (char * Position list) list) (antinodes: (int * int) list) =
    antennas
    |> List.map (snd)
    |> List.filter (fun f -> f.Length > 1)
    |> List.concat
    |> List.map (fun pos -> (pos.x, pos.y))
    |> List.append antinodes
    |> List.distinct
    |> List.length

exampleInput
|> solveAntinodes ([exampleInput.Length;exampleInput.[0].Length] |> List.max) exampleAntennas
|> calculateAntinodePart2 exampleAntennas
|> printfn "Example answer 2: %d"

let input = parseInput "./input/day08.txt"
let antennas = antennaPositions input
let antinodes = input |> solveAntinodes 1 antennas
antinodes
|> List.length
|> printfn "Answer 1: %d"

input
|> solveAntinodes ([input.Length;input.[0].Length] |> List.max) antennas
|> calculateAntinodePart2 antennas
|> printfn "Answer 2: %d"