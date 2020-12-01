open System.IO

let loadIntegers filename =
    File.ReadAllLines filename
    |> Array.map int

let findPairWithSum sum ints =
    let map = Set.ofArray ints
    ints
    |> Array.tryFind (fun i -> map |> Set.contains (sum - i))
    |> function
        | Some i -> Some (i, sum - i)
        | None -> None

[<EntryPoint>]
let main _ =
    let ints = loadIntegers "./input.txt"
    printfn "Loaded %i numbers" ints.Length

    let pair = findPairWithSum 2020 ints
    printfn "Pair: %A" pair

    match pair with
    | Some (x, y) -> printfn "Product: %i" (x * y)
    | None -> ()
    0

