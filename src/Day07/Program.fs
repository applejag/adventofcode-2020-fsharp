open System.IO
open System.Text.RegularExpressions

let readLines filename =
    File.ReadAllLines filename

/// Example: dim tan bags contain 3 shiny teal bags, 5 bright white bags, 4 striped bronze bags.
/// => { BagColor = "dim tan"; Holds = map [("shiny teal", 3); ("bright white",5); ("striped bronze"; 4)]
type LuggageRule = {
    BagColor: string
    Holds: Map<string, int>
}

let luggageRuleRegex = Regex(@"(^|\d+ )(.*?) bags?", RegexOptions.Compiled)
let parseLuggageRule line =
    let matches = luggageRuleRegex.Matches line
    {
        BagColor = (Seq.item 0 matches).Groups.[2].Value
        Holds = matches
            |> Seq.skip 1
            |> Seq.map (fun m -> m.Groups.[2].Value, int m.Groups.[1].Value)
            |> Map.ofSeq
    }

let addToMapOfSets key item map =
    match Map.tryFind key map with
    | Some items -> map |> Map.add key (Set.add item items)
    | None -> map |> Map.add key (Set [item])

let calcParentalMappings rules =
    rules
    |> Array.fold (fun map rule ->
        Map.toSeq rule.Holds
        |> Seq.map fst
        |> Seq.fold (fun map bag -> map |> addToMapOfSets bag rule.BagColor) map
    ) Map.empty

let findBagsThatEventuallyContains soughtColor parentalMap =
    let rec find map col =
        match Map.tryFind col map with
        | None -> Set [col]
        | Some bags ->
            bags
            |> Seq.map (find map)
            |> Seq.reduce Set.union
            |> Set.add col

    find parentalMap soughtColor
    |> Set.remove soughtColor

let sumBagsDeep bagColor rules =
    let map =
        rules
        |> Array.Parallel.map (fun r -> (r.BagColor, r.Holds |> Map.toSeq))
        |> Map.ofArray
    let rec flattenBagSums col mult =
        seq {
            match Map.tryFind col map with
            | None -> ()
            | Some holdings ->
                yield! (holdings |> Seq.map (snd >> (*) mult))
                for (innerCol, count) in holdings do
                    yield! flattenBagSums innerCol (count * mult)
        }
    flattenBagSums bagColor 1
    |> Seq.sum

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let rules = Array.Parallel.map parseLuggageRule lines
    printfn "Parsed rules for %i different bag colors" (rules |> Array.distinctBy (fun r -> r.BagColor) |> Array.length)

    let parentalMap = calcParentalMappings rules
    //parentalMap
    //|> Map.iter (printfn " %20s is found in %O")

    let soughtColor = "shiny gold"
    let bags = findBagsThatEventuallyContains soughtColor parentalMap
    printfn ""
    printfn "Part 1:"
    printfn " Number of bag colors that eventually contain '%s': %i" soughtColor bags.Count

    //let firstWordRegex = Regex(@"^\w+", RegexOptions.Compiled)
    //bags
    //|> Seq.groupBy (fun col -> (firstWordRegex.Match col).Value)
    //|> Seq.iter (fun (g, cols) -> printfn "%s %i:\n  %s" g (Seq.length cols) <| String.concat ", " cols)

    printfn ""
    printfn "Part 2:"
    printfn " Deep number of bags needed for '%s': %i" soughtColor (sumBagsDeep soughtColor rules)
    0
