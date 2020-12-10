open System.IO

let readLines filename =
    File.ReadAllLines filename

let findPairWithSum sum ints =
    let intsSet = Set.ofArray ints
    ints
    |> Array.tryFind (fun i -> intsSet |> Set.contains (sum - i))
    |> function
        | Some i -> Some [ i; sum - i ]
        | None -> None

let findTripletWithSum sum ints =
    let createSetWithValueAdded skip n =
        (n, Seq.skip skip ints |> Seq.map ((+) n) |> Set.ofSeq)

    let chooseTriplet i intAddedToSet setWithIntAdded =
        if intAddedToSet <> i && Set.contains (sum - i) setWithIntAdded then
            Some [ intAddedToSet; i; sum - i - intAddedToSet ]
        else
            None

    let sets = ints |> Array.mapi createSetWithValueAdded

    let tryPickValidTriplet i =
        sets
        |> Array.tryPick (fun intAndAlteredSet -> chooseTriplet i <|| intAndAlteredSet)

    ints
    |> Array.tryPick tryPickValidTriplet

let printResults result =
    printfn " Result: %A" result

    match result with
    | Some values ->
        printfn " Sum: %i" <| List.reduce (+) values
        printfn " Product: %i" <| List.reduce (*) values
    | None -> ()

[<EntryPoint>]
let main args =
    let filename = Array.tryItem 0 args |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename
    let ints = lines |> Array.map int

    printfn ""
    printfn "Part 1:"
    printfn " Pairs:"
    printResults <| findPairWithSum 2020 ints

    printfn ""
    printfn "Part 2:"
    printfn " Triplets:"
    printResults <| findTripletWithSum 2020 ints

    0
