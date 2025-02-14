// Macro to get running time
#time 

type FileBlockKind = | File | Free
type DiskBlock = {
    id: int
    size: int
    kind: FileBlockKind
}

let parseInput (filePath : string)  =
    System.IO.File.ReadAllLines filePath
    |> Array.head
    |> (fun (s: string) -> s.ToCharArray())
    |> Array.mapi (fun i c -> 
        let cSize = int c - int '0'
        match i % 2 with 
        | 0 -> { id = (i / 2); size = cSize; kind = File }
        | _ -> { id = 0; size = cSize; kind = Free })
    |> List.ofArray

let stretchFileFormat (diskMap: DiskBlock list) =
    diskMap
    |> List.map (fun db -> 
        [1..db.size] |> List.map (fun _ -> { id = db.id; size = 1; kind = db.kind}))
    |> List.concat

let printCurrentDiskMap (diskMap: DiskBlock list) = 
    diskMap
    |> List.map (fun db -> 
        match db.kind with
        | File -> (int '0' + db.id) |> char
        | Free -> '.')
    |> Array.ofList
    |> System.String
    |> printfn "%A" 

let fillFromRightToLeft (diskMap: DiskBlock list) =
    List.foldBack (fun (db: DiskBlock) (modifiedDiskMap: DiskBlock list) -> 
        let firstFree = modifiedDiskMap |> List.findIndex (fun dbToMatch -> dbToMatch.kind = Free)
        let isFreeTail = modifiedDiskMap |> List.skip firstFree |> List.forall (fun f -> f.kind = Free)
        if isFreeTail then modifiedDiskMap
        else 
            match db.kind with 
            | Free -> modifiedDiskMap // Don't touch free blocks
            | File -> // Try to move to left
                let lastSimilar = modifiedDiskMap |> List.findIndexBack (fun dbToMatch -> dbToMatch = db)
                let updatedMap = 
                    modifiedDiskMap 
                    |> List.removeAt firstFree 
                    |> List.insertAt firstFree db
                    |> List.removeAt lastSimilar
                    |> List.insertAt lastSimilar { id = 0; size = 1; kind = Free }
                updatedMap
     ) diskMap diskMap

let calculateChecksum (diskMap: DiskBlock list) =
    diskMap
    |> List.mapi (fun i db -> 
        match db.kind with 
        | Free -> 0 |> int64
        | File -> (i |> int64) * (db.id |> int64))
    |> List.sum

let exampleInput = parseInput "./input/day09_example.txt"
exampleInput
|> stretchFileFormat
|> fillFromRightToLeft
|> calculateChecksum
|> printfn "Example answer 1: %d"

let input = parseInput "./input/day09.txt"
input
|> stretchFileFormat
|> fillFromRightToLeft
|> calculateChecksum
|> printfn "Answer 1: %d"