open System.IO

type Line = int * int

type Rect = {
    X: Line
    Y: Line
}

type Direction =
    | Front
    | Back
    | Left
    | Right

let readLines filename =
    File.ReadAllLines filename

let parseLine line =
    line
    |> Seq.map (function
        | 'F' -> Front
        | 'B' -> Back
        | 'L' -> Left
        | 'R' -> Right
        | c -> failwithf "Unknown char '%c'" c
    )
    |> Array.ofSeq

(*
 * Example:
     * 0 7
     * 0 3
     * 0 1
     * 0 0
 *)
let takeFirstHalf min max =
    let range = max - min + 1
    if range <= 1 then
        (min, min)
    else
        (min, max - range / 2)

(*
 * Example:
     * 0 7
     * 4 7
     * 6 7
     * 7 7
 *)
let takeSecondHalf min max =
    let range = max - min + 1
    if range <= 1 then
        (max, max)
    else
        (min + range / 2, max)

let partition rect dir =
    match dir with
    | Front -> { rect with Y = takeFirstHalf <|| rect.Y }
    | Back -> { rect with Y = takeSecondHalf <|| rect.Y }
    | Left -> { rect with X = takeFirstHalf <|| rect.X }
    | Right -> { rect with X = takeSecondHalf <|| rect.X }

let (|SinglePoint|_|) = function
    | { X = (x1,x2); Y = (y1,y2) } when x1=x2 && y1=y2 -> Some (x1, y1)
    | _ -> None

let calculatePoint dirs =
    dirs
    |> Array.fold partition { X = (0,7); Y = (0,127) }
    |> function
        | SinglePoint point -> Ok point
        | rect -> Error (sprintf "Dirs '%A' did not resolve to a single point: %A" dirs rect)

let calculateSeadId = function
    | x, y -> x + y * 8

let resultToOption = function
    | Ok v -> Some v
    | Error _ -> None

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let parsedLines = Array.Parallel.map parseLine lines
    let pointsResults = Array.Parallel.map calculatePoint parsedLines
    let points = pointsResults |> Array.choose resultToOption

    printfn "Found %i/%i points from all seats definitions" points.Length pointsResults.Length
    let highestSeatId = points |> Array.Parallel.map calculateSeadId |> Array.max
    printfn "Highest sead ID: %i" highestSeatId

    0
