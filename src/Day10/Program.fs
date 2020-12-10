open System.IO

let readLines filename =
    File.ReadAllLines filename

let calcAndPrintDifferences joltages =
    let differences = joltages |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
    let countPerDiff = differences |> Array.countBy id |> Map.ofArray
    printfn " Differences: %A" countPerDiff
    let numOf1Diffs = countPerDiff.[1]
    let numOf3Diffs = countPerDiff.[3]
    printfn " 1 diffs * 3 diffs = %i" (numOf1Diffs * numOf3Diffs)

[<EntryPoint>]
let main args =
    let filename = Array.tryItem 0 args |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines" lines.Length

    let adapters = lines |> Array.map int
    let highest = adapters |> Array.max
    let joltages = adapters |> Array.append [| 0; highest + 3 |] |> Array.sort

    printfn ""
    printfn "Part 1:"
    calcAndPrintDifferences joltages

    0
