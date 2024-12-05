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

let filterPrintableOrders (rules: int list list) (updates: int list list) =
    let filterRules (update: int list) =
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
    |> List.map (fun update -> 
        let result = pagePrintable update update (filterRules update)
        (update,result))
    |> List.filter snd
    |> List.map fst
    |> List.map (fun u -> u.[u.Length / 2])
    |> List.sum

let (exampleRules, exampleUpdates) = parseInput "./input/day05_example.txt" 
filterPrintableOrders exampleRules exampleUpdates
|> printfn "Example answer 1: %A"

let (rules, updates) = parseInput "./input/day05.txt" 
filterPrintableOrders rules updates
|> printfn "Example answer 1: %A"