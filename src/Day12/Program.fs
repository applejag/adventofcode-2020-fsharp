open System.IO

let readLines filename =
    File.ReadAllLines filename

type Instruction =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of int
    | Right of int
    | Forward of int

let parseInstruction (line: string) =
    match line.[0], int line.[1..] with
    | 'N', n -> North n
    | 'S', n -> South n
    | 'E', n -> East n
    | 'W', n -> West n
    | 'L', n -> Left n
    | 'R', n -> Right n
    | 'F', n -> Forward n
    | _ -> failwithf "Unable to parse instruction '%s'" line

/// Rotates degrees and enforces that the result is in the range 0..359
let rotateDegrees delta degrees =
    ((degrees + delta) % 360 + 360) % 360

type ShipPosition = {
    Direction: int
    East: int
    North: int
}
with
    static member Empty = { Direction = 0; East = 0; North = 0 }

let translateForward position distance =
    match position.Direction with
    | 0 -> { position with East = position.East + distance }
    | 90 -> { position with North = position.North - distance }
    | 180 -> { position with East = position.East - distance }
    | 270 -> { position with North = position.North + distance }
    | d -> failwithf "Invalid direction on position: %i" d

let translatePosition position instruction =
    match instruction with
    | North n -> { position with North = position.North + n }
    | South n -> { position with North = position.North - n }
    | East n -> { position with East = position.East + n }
    | West n -> { position with East = position.East - n }
    | Left n -> { position with Direction = rotateDegrees -n position.Direction }
    | Right n -> { position with Direction = rotateDegrees n position.Direction }
    | Forward n -> translateForward position n

let manhattanDistance position =
    abs position.East + abs position.North

[<EntryPoint>]
let main argv =
    let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let instructions = Array.Parallel.map parseInstruction lines

    printfn ""
    printfn "Part 1:"
    let pos = Array.fold translatePosition ShipPosition.Empty instructions
    printfn " Resulting position: east %i, north %i, direction %i" pos.East pos.North pos.Direction
    printfn " Manhattan distance: %i" (manhattanDistance pos)

    //printfn ""
    //printfn "Part 2:"

    0
