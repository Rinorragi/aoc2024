// Macro to get running time
#time 

// Newline
let nl = "\n"


let parseString (s: string) (parseString: string)=
    s.Split(nl, System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map (fun ss -> 
        ss.Split(parseString,System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map int)

let parseInput (filePath : string)  =
    let inputs =
        System.IO.File.ReadAllText filePath
        // Replace CRLF to only LF (copy+paste and input in different format)
        |> fun s -> s.Replace("\r\n", nl)
        |> fun s -> s.Split(nl+nl, System.StringSplitOptions.RemoveEmptyEntries)
    (parseString inputs.[0] "|", parseString inputs.[1] ",")

let filterRules (rules: int list list) (update: int list) =
    rules 
    |> List.map (fun rule -> 
        let intersectCount = 
            update 
            |> Set.ofList 
            |> Set.intersect (rule 
            |> Set.ofList) |> Set.count 
        match intersectCount with 
        | 2 -> Some(rule)
        | _ -> None)
    |> List.filter Option.isSome
    |> List.map Option.get

let filterPrintableOrders (rules: int list list) (updates: int list list) =
    let rec pagePrintable (originalUpdate: int list) (update : int list) (updateRules: int list list) =
        let updateNumber = update |> List.head
        if update.Length = 1 
            && updateRules |> List.map List.last |> List.contains updateNumber then true
        else 
            let updateTail = update |> List.tail
            let updateBeforeList = originalUpdate |> List.take (originalUpdate.Length - update.Length)
            
            let matchingRules = 
                updateRules
                |> List.filter (fun f -> 
                    f |> List.head = updateNumber && updateTail |> List.contains (f |> List.last))
            let conflictingRules =
                updateRules
                |> List.filter (fun (f: int list) -> 
                    f |> List.head = updateNumber && updateBeforeList |> List.contains (f |> List.last))
            if conflictingRules.Length > 0
            then false 
            elif matchingRules.Length > 0 
            then pagePrintable originalUpdate updateTail updateRules
            else false
    updates
    |> List.map (fun update -> (update,pagePrintable update update (filterRules rules update)))

let fixNonPrintableOrders (rules: int list list) (updates: int list list) =
    let rec pagePrintable (update : int list) (updateIndex: int) (updateRules: int list list) =
        let updateNumber = update.[updateIndex]
        if updateIndex = update.Length - 1 
            && updateRules |> List.map List.last |> List.contains updateNumber 
            && updateRules |> List.map List.head |> List.contains updateNumber |> not
        then update
        else 
            let updateTail = update |> List.tail
            let updateBeforeList = update |> List.take updateIndex
            
            let matchingRules = 
                updateRules
                |> List.filter (fun f -> 
                    f |> List.head = updateNumber && updateTail |> List.contains (f |> List.last))
            let conflictingRules =
                updateRules
                |> List.filter (fun (f: int list) -> 
                    f |> List.head = updateNumber && updateBeforeList |> List.contains (f |> List.last))
            if conflictingRules.Length > 0
            then 
                pagePrintable (update |> List.removeAt updateIndex |> List.insertAt (updateIndex - 1) updateNumber) 0 updateRules 
            else pagePrintable update (updateIndex + 1) updateRules
    updates
    |> List.map (fun update -> (pagePrintable update 0 (filterRules rules update)))

let calculateResult (updates: int list list) =
    updates  
    |> List.map (fun u -> u.[u.Length / 2])
    |> List.sum

let (exampleRules, exampleUpdates) = parseInput "./input/day05_example.txt" 
let exampleResult = filterPrintableOrders exampleRules exampleUpdates
exampleResult
|> List.filter snd
|> List.map fst
|> calculateResult
|> printfn "Example answer 1: %d"

let (rules, updates) = parseInput "./input/day05.txt" 
let result = filterPrintableOrders rules updates
result
|> List.filter snd
|> List.map fst
|> calculateResult
|> printfn "Answer 1: %d"

exampleResult
|> List.filter (snd >> not)
|> List.map fst
|> fixNonPrintableOrders exampleRules
|> calculateResult
|> printfn "Example answer 2: %d"

result
|> List.filter (snd >> not)
|> List.map fst
|> fixNonPrintableOrders rules
|> calculateResult
|> printfn "Answer 2: %d"