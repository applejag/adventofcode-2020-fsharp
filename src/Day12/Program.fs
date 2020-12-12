open System.IO

let readLines filename =
    File.ReadAllLines filename

type Direction =
    | Deg0
    | Deg90
    | Deg180
    | Deg270
with
    static member (~-) dir =
        match dir with
        | Deg0 -> Deg0
        | Deg90 -> Deg270
        | Deg180 -> Deg180
        | Deg270 -> Deg90

    override this.ToString() =
        match this with
        | Deg0 -> "0° (east)"
        | Deg90 -> "90° (south)"
        | Deg180 -> "180° (west)"
        | Deg270 -> "270° (north)"

module Direction =
    let ofInt deg =
        match (deg % 360 + 360) % 360 with
        | 0 -> Deg0
        | 90 -> Deg90
        | 180 -> Deg180
        | 270 -> Deg270
        | _ -> failwithf "Invalid degrees: %i" deg

    let toInt dir =
        match dir with
        | Deg0 -> 0
        | Deg90 -> 90
        | Deg180 -> 180
        | Deg270 -> 270

    let rotate delta direction =
        ((direction |> toInt) + (delta |> toInt)) |> ofInt

type Instruction =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of Direction
    | Right of Direction
    | Forward of int

let parseInstruction (line: string) =
    match line.[0], int line.[1..] with
    | 'N', n -> North n
    | 'S', n -> South n
    | 'E', n -> East n
    | 'W', n -> West n
    | 'L', n -> Left (Direction.ofInt n)
    | 'R', n -> Right (Direction.ofInt n)
    | 'F', n -> Forward n
    | _ -> failwithf "Unable to parse instruction '%s'" line

type DirectionalShip = {
    Direction: Direction
    East: int
    North: int
}
with
    member this.Position = this.East, this.North
    static member Start = { Direction = Deg0; East = 0; North = 0 }

let translateForward position distance =
    match position.Direction with
    | Deg0 -> { position with East = position.East + distance }
    | Deg90 -> { position with North = position.North - distance }
    | Deg180 -> { position with East = position.East - distance }
    | Deg270 -> { position with North = position.North + distance }

let translatePosition position instruction =
    match instruction with
    | North n -> { position with North = position.North + n }
    | South n -> { position with North = position.North - n }
    | East n -> { position with East = position.East + n }
    | West n -> { position with East = position.East - n }
    | Left d -> { position with Direction = Direction.rotate -d position.Direction }
    | Right d -> { position with Direction = Direction.rotate d position.Direction }
    | Forward n -> translateForward position n

type WaypointedShip = {
    East: int
    North: int
    WaypointEast: int
    WaypointNorth: int
}
with
    member this.Position = this.East, this.North
    static member Start = { East = 0; North = 0; WaypointEast = 10; WaypointNorth = 1 }

let translateForwardWaypointed (position: WaypointedShip) distance =
    { position with
        East = position.East + position.WaypointEast * distance
        North = position.North + position.WaypointNorth * distance
    }

let rotateWaypoint position direction =
    match direction with
    | Deg0 -> position
    | Deg90 ->
        // (x, y) <- (y, -x)
        { position with
            WaypointEast = position.WaypointNorth
            WaypointNorth = -position.WaypointEast
        }
    | Deg180 -> 
        // (x, y) <- (-x, -y)
        { position with
            WaypointEast = -position.WaypointEast
            WaypointNorth = -position.WaypointNorth
        }
    | Deg270 ->
        // (x, y) <- (-y, x)
        { position with
            WaypointEast = -position.WaypointNorth
            WaypointNorth = position.WaypointEast
        }

let translatePositionWaypointed position instruction =
    match instruction with
    | North n -> { position with WaypointNorth = position.WaypointNorth + n }
    | South n -> { position with WaypointNorth = position.WaypointNorth - n }
    | East n -> { position with WaypointEast = position.WaypointEast + n }
    | West n -> { position with WaypointEast = position.WaypointEast - n }
    | Left d -> rotateWaypoint position -d
    | Right d -> rotateWaypoint position d
    | Forward n -> translateForwardWaypointed position n

let manhattanDistance (x, y) =
    abs x + abs y

[<EntryPoint>]
let main argv =
    let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let instructions = Array.Parallel.map parseInstruction lines

    printfn ""
    printfn "Part 1:"
    let pos1 = Array.fold translatePosition DirectionalShip.Start instructions
    printfn " Resulting position: east %i, north %i, direction %O" pos1.East pos1.North pos1.Direction
    printfn " Manhattan distance: %i" (manhattanDistance pos1.Position)

    printfn ""
    printfn "Part 2:"
    let pos2 = Array.fold translatePositionWaypointed WaypointedShip.Start instructions
    printfn " Resulting position: east %i, north %i" pos2.East pos2.North
    printfn " Resulting waypoint: east %i, north %i" pos2.WaypointEast pos2.WaypointNorth
    printfn " Manhattan distance: %i" (manhattanDistance pos2.Position)

    0
