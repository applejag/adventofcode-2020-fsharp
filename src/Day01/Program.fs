open System.IO

let loadIntegers filename =
    File.ReadAllLines filename
    |> Array.map int

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

[<EntryPoint>]
let main _ =
    let ints = loadIntegers "./input.txt"
    printfn "Loaded %i numbers" ints.Length

    let result = findTripletWithSum 2020 ints
    printfn "Result: %A" result

    match result with
    | Some values ->
        printfn "Sum: %i" <| List.reduce (+) values
        printfn "Product: %i" <| List.reduce (*) values
    | None -> ()
    0
