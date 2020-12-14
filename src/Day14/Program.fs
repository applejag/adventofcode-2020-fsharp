open System.IO
open System.Text.RegularExpressions

let readLines filename =
    File.ReadAllLines filename

type MaskBit =
    | IgnoreBit
    | Override1
    | Override0

type Mask = {
    Value: uint64
    MetaMask: uint64
}
with
    static member empty = { Value = 0uL; MetaMask = 0uL }

type Instruction =
    | MaskAssignment of Mask
    | MemoryAssignment of int * uint64

let parseMaskBit = function
    | 'X' -> IgnoreBit
    | '1' -> Override1
    | '0' -> Override0
    | c -> failwithf "Invalid mask bit '%c'" c

let parseMask (str: string) =
    str.ToCharArray()
    |> Array.map parseMaskBit
    |> Array.rev
    |> Array.truncate 36
    |> Array.indexed
    |> Array.fold (fun mask (n, maskBit) ->
        match maskBit with
        | Override1 ->
            {
                Value = mask.Value ||| (1uL <<< n)
                MetaMask = mask.MetaMask ||| (1uL <<< n)
            }
        | Override0 ->
            {
                Value = mask.Value &&& (~~~(1uL <<< n))
                MetaMask = mask.MetaMask ||| (1uL <<< n)
            }
        | IgnoreBit -> mask
    ) Mask.empty

let lineRegex = Regex(@"mask = ([X01]{36})|mem\[(\d+)\] = (\d+)", RegexOptions.Compiled)
let parseLine line =
    let m = lineRegex.Match line
    if not m.Success then
        failwithf "Unable to parse line '%s'" line
    elif m.Groups.[1].Success then
        m.Groups.[1].Value
        |> parseMask
        |> MaskAssignment
    else
        MemoryAssignment (int m.Groups.[2].Value, uint64 m.Groups.[3].Value)

type State = {
    Mask: Mask
    Memory: Map<int, uint64>
}
with
    static member empty = { Mask = Mask.empty; Memory = Map.empty }

let applyMask mask n =
    n
    // Set 1's
    |> (|||) (mask.MetaMask &&& mask.Value)
    // Unset 0's
    |> (&&&) ((~~~mask.MetaMask) ||| mask.Value)

let instructionFolder state instruction =
    match instruction with
    | MaskAssignment m -> { state with Mask = m }
    | MemoryAssignment (loc, value) -> { state with Memory = Map.add loc (applyMask state.Mask value) state.Memory }

/// For testing purposes.
let uint64ToBinary i =
    let rec appendNextBit i (sb: System.Text.StringBuilder) =
        match i with
        | 0uL | 1uL -> sb.Append i
        | _ -> (appendNextBit (i / 2uL) sb).Append (i % 2uL)
    System.Text.StringBuilder()
    |> appendNextBit i
    |> string

[<EntryPoint>]
let main argv =
    let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename
    let instructions = lines |> Array.Parallel.map parseLine

    printfn ""
    printfn "Part 1:"
    let finalState = instructions |> Array.fold instructionFolder State.empty
    let memSum = finalState.Memory |> Map.toSeq |> Seq.sumBy snd
    printfn " Sum of all memory: %i" memSum

    //printfn ""
    //printfn "Part 2:"

    0
