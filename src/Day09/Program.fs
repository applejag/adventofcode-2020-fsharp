open System.IO

let readLines filename =
    File.ReadAllLines filename

module List =
    /// In = [ 1; 2; 3 ]
    /// Out = [ 1,2; 1,3; 2,3 ]
    ///
    /// In = [ a; b; c; d ]
    /// Out = [ a,b; a,c; a,d; b,c; b,d; c,d ]
    ///
    /// In = [ a ]
    /// Out = []
    let rec permutationPairs source =
        match source with
        | [] -> []
        | head :: tail -> List.collect (fun v -> [head, v]) tail @ permutationPairs tail

let containsSumOfTwoUniqueInSpan expectedSum arr =
    List.ofArray arr
    |> List.permutationPairs
    |> List.exists (fun (a, b) -> a + b = expectedSum)

let validateIntAtIndex (ints: int64[]) index integer =
    if index < 25 then
        true
    else
        containsSumOfTwoUniqueInSpan integer ints.[index-25..index-1]

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let ints = Array.map int64 lines
    let validated = Array.Parallel.mapi (validateIntAtIndex ints) ints

    let firstInvalidIndex = validated |> Array.tryFindIndex ((=) false)
    printfn "First invalid at index: %A" firstInvalidIndex
    printfn "First invalid integer: %A" (firstInvalidIndex |> Option.map (Array.get ints))

    0
