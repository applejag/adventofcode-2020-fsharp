open System.IO

let readLines filename = File.ReadAllLines filename

let splitIntoGroups lines =
    Array.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail
    ) [[]] lines

let foldListToSet charsList =
    charsList
    |> List.map Set
    |> List.fold Set.union Set.empty

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let groups = splitIntoGroups lines |> Array.ofList
    printfn "Found %i groups" groups.Length

    let answersPerGroups = Array.map foldListToSet groups
    let sumOfAnswersPerGroups =
        answersPerGroups
        |> Array.sumBy Set.count

    printfn "Sum of all groups answers: %i" sumOfAnswersPerGroups

    0 // return an integer exit code
