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
    let createSetWithValueAdded skip value =
        (value, Seq.skip skip ints |> Seq.map ((+) value) |> Set.ofSeq)

    let sets = ints |> Array.mapi createSetWithValueAdded

    ints
    |> Array.tryPick (fun i2 ->
        sets
        |> Array.tryPick (fun (i, s) ->
            if i <> i2 && Set.contains (sum - i2) s then
                Some [ i; i2; sum - i2 - i ]
            else
                None
        )
    )

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
