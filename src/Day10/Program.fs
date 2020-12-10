open System.IO

let readLines filename =
    File.ReadAllLines filename

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let adapters = lines |> Array.map int
    let highest = adapters |> Array.max

    let joltages = adapters |> Array.append [| 0; highest + 3 |] |> Array.sort
    let differences = joltages |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
    let countPerDiff = differences |> Array.countBy id |> Map.ofArray

    printfn "Differences: %A" countPerDiff
    let numOf1Diffs = countPerDiff.[1]
    let numOf3Diffs = countPerDiff.[3]

    printfn "1 diffs * 3 diffs = %i" (numOf1Diffs * numOf3Diffs)

    0
