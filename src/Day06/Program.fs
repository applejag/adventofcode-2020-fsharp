open System.IO

let readLines filename = File.ReadAllLines filename

let splitIntoGroups lines =
    Array.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail
    ) [[]] lines

let foldListToSet initState folder charsList =
    charsList
    |> List.map Set
    |> List.fold folder initState

let sumSetCounts s = Array.sumBy Set.count s

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let groups = splitIntoGroups lines |> Array.ofList
    printfn "Found %i groups" groups.Length

    let unionAnswers = groups |> Array.map (foldListToSet Set.empty Set.union)
    let sumOfUnion = unionAnswers |> sumSetCounts
    printfn "Sum of union answers per group: %i" sumOfUnion

    let allQuestions = unionAnswers |> Array.reduce Set.union
    let intersectAnswers = groups |> Array.map (foldListToSet allQuestions Set.intersect)
    let sumOfIntersect = intersectAnswers |> sumSetCounts
    printfn "Sum of intersect answers per group: %i" sumOfIntersect

    0
