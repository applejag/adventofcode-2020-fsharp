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

let foldGroupsToSets initState folder groups =
    groups
    |> Array.Parallel.map (foldListToSet initState folder)

let sumSetCounts s = Array.sumBy Set.count s

let sumFoldedGroups initState folder groups =
    groups
    |> foldGroupsToSets initState folder
    |> sumSetCounts

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let groups = splitIntoGroups lines |> Array.ofList
    printfn "Found %i groups" groups.Length

    let sumOfUnion = groups |> sumFoldedGroups Set.empty Set.union
    printfn "Sum of union answers per group: %i" sumOfUnion

    let allQuestions = set { 'a'..'z' }
    let sumOfIntersect = groups |> sumFoldedGroups allQuestions Set.intersect
    printfn "Sum of intersect answers per group: %i" sumOfIntersect

    0
