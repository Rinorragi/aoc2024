// Macro to get running time
#time 

type TravelDirection = | Up | Down | Left | Right
type GuardianSpecialPositions = | Start | Obstacle | Land 
type GuardianPosition = {
    x: int
    y: int
    c: char
    posType: GuardianSpecialPositions
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
                | '#' -> Obstacle
                | '^' | 'v' | '>' | '<' -> Start
                | _ -> Land
        }))

let trackGuardianRoute (inputData: GuardianPosition list list) =
    let maxX = inputData.Head.Length - 1
    let maxY = inputData.Length - 1
    let startPosition = 
        inputData 
        |> List.concat 
        |> List.filter (fun gp -> gp.posType = Start) 
        |> List.exactlyOne
    let obstacles = 
        inputData 
        |> List.concat 
        |> List.filter (fun gp -> gp.posType = Obstacle) 
        |> List.map (fun gp -> ((gp.y,gp.x),gp)) 
        |> dict
    let rec travel (dir: TravelDirection) (y: int, x:int) (visitedPosList: GuardianPosition list) =
        if y < 0 || x < 0 || y > maxY || x > maxX then visitedPosList
        else
            let nextPos = 
                match dir with
                | Up -> (y-1,x)
                | Down -> (y+1,x)
                | Right -> (y,x+1)
                | Left -> (y,x-1)
            if obstacles.ContainsKey nextPos
            then 
                let newDir = 
                    match dir with 
                    | Up -> Right
                    | Right -> Down
                    | Down -> Left
                    | Left -> Up
                travel newDir (y,x) visitedPosList
            else
                let newVisitedPosition = visitedPosList @ [inputData.[y].[x]]
                travel dir nextPos newVisitedPosition 
    travel Up (startPosition.y,startPosition.x) []

parseInput "./input/day06_example.txt"
|> trackGuardianRoute
|> List.distinct
|> List.length 
|> printfn "Example answer 1: %d"


parseInput "./input/day06.txt"
|> trackGuardianRoute
|> List.distinct
|> List.length 
|> printfn "Answer 1: %d"