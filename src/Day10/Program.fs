open System.IO

let readLines filename =
    File.ReadAllLines filename

let calcAndPrintDifferences joltages =
    let differences = joltages |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
    let countPerDiff = differences |> Array.countBy id |> Map.ofArray
    printfn " Differences: %A" countPerDiff
    printfn " 1 diffs * 3 diffs = %i" (countPerDiff.[1] * countPerDiff.[3])

let listPossibleJoltageSources fromJoltage joltagesSet =
    Set.ofArray [| fromJoltage-3..fromJoltage-1 |]
    |> Set.intersect joltagesSet

module Map =
    let findMultiple keys table =
        Seq.map (fun key -> Map.find key table) keys

let addSumOfPossiblePaths joltagesSet joltagesPathsMap joltage =
    let sum =
        joltagesPathsMap
        |> Map.findMultiple (listPossibleJoltageSources joltage joltagesSet)
        |> Seq.sum
    Map.add joltage sum joltagesPathsMap

let calcPossiblePaths joltagesSorted =
    (* Rules:
        Joltage         Number of paths
        0               1
        n               sum(paths to possible joltages)
    *)
    joltagesSorted
    |> Array.except [| 0 |]
    |> Array.fold (addSumOfPossiblePaths (Set.ofArray joltagesSorted)) (Map.ofList [(0,1L)])

[<EntryPoint>]
let main args =
    let filename = Array.tryItem 0 args |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines" lines.Length

    let adapters = lines |> Array.map int
    let highest = adapters |> Array.max
    let joltagesSorted = adapters |> Array.append [| 0; highest + 3 |] |> Array.sort

    printfn ""
    printfn "Part 1:"
    calcAndPrintDifferences joltagesSorted

    printfn ""
    printfn "Part 2:"
    let paths = calcPossiblePaths joltagesSorted
    printfn " Number of possible paths: %i" paths.[highest]
    printfn " Number of possible paths (formatted): %s" (paths.[highest].ToString "#,0")

    0
