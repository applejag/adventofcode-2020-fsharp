open System.IO
open System.Runtime.CompilerServices

let readLines filename =
    File.ReadAllLines filename

type Operation =
    | Nop of int
    | Acc of int
    | Jmp of int

let splitOp (line: string) =
    // Using String.item[Range] is more forgiving than String.Substring
    // as it returns empty string when indices are out of bounds
    line.[0..2], line.[4..]

let parseOp line =
    match splitOp line with
    | "nop", nop -> Nop (int nop)
    | "acc", acc -> Acc (int acc)
    | "jmp", jmp -> Jmp (int jmp)
    | _ -> failwithf "Unable to parse op: '%s'" line

[<IsReadOnly; Struct>]
type ProgramState(pc: int, acc: int) =
    member _.Pc = pc
    member _.Acc = acc
    static member Empty = ProgramState(0,0)
    override this.ToString() = sprintf "{ Pc = %i; Acc = %i }" this.Pc this.Acc

let applyOp (state: ProgramState) op =
    match op with
    | Nop _ -> ProgramState(state.Pc + 1, state.Acc)
    | Acc acc -> ProgramState(state.Pc + 1, state.Acc + acc)
    | Jmp jmp -> ProgramState(state.Pc + jmp, state.Acc)

let pair v = (v, v)

let walkProgram ops =
    Seq.unfold (fun (state: ProgramState) ->
        Array.tryItem state.Pc ops
        |> Option.map (applyOp state >> pair)
    ) ProgramState.Empty

type Continuation<'State> =
    | Continue
    | Next of 'State

module Seq =
    let foldFind chooser (state: 'State) (source: seq<'T>) =
        let mutable s = state
        source
        |> Seq.tryPick (fun item ->
            match chooser s item with
            | Continue -> Some item
            | Next state -> s <- state; None
        )

let stopWhenLoopDetected (states: seq<ProgramState>) =
    states
    |> Seq.foldFind (fun pcs state ->
        if Set.contains state.Pc pcs then
            Continue
        else
            Next (Set.add state.Pc pcs)
    ) Set.empty

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let ops = Array.Parallel.map parseOp lines
    printfn "Parsed %i ops: %A" ops.Length ops
    printfn ""

    let statesSeq = walkProgram ops
    let loopDetectedAt = stopWhenLoopDetected statesSeq
    printfn "Detected loop at: %A" loopDetectedAt

    0
