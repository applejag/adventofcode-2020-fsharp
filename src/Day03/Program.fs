open System.IO

type Node =
    | Tree
    | Empty

type Coordinate = {
    Left: int
    Top: int
}

let move dx dy pos =
    { pos with
        Left = pos.Left + dx
        Top = pos.Top + dy
    }

type Map =
    {
        Grid: Node[,]
        Position: Coordinate
    }
    static member GetNodeAt pos grid =
        let i1 = pos.Left % (Array2D.length1 grid)
        grid.[i1, pos.Top]

    member this.CurrentNode =
        Map.GetNodeAt this.Position this.Grid

    member this.IsOutOfBounds =
        this.Position.Top < 0
        || this.Position.Top >= (Array2D.length2 this.Grid)
        || this.Position.Left < 0

    member this.WithMove dx dy =
        { this with Position = move dx dy this.Position }

let readLines filename =
    File.ReadAllLines filename

let parseChar = function
    | '#' -> Tree
    | '.' -> Empty
    | c -> raise (System.Exception(sprintf "Invalid char '%c'" c))

let parseLine (line: string) =
    line.ToCharArray()
    |> Array.map parseChar

let jaggedTo2D (jagged: 'a[][]) =
    let w = jagged.[0].Length
    let h = jagged.Length
    let init x y = jagged.[y].[x]

    Array2D.init w h init

let walkTilOutOfBounds dx dy (map: Map) =
    map |> Seq.unfold (fun map ->
        if map.IsOutOfBounds then
            None
        else
            Some ((map.Position, map.CurrentNode), map.WithMove dx dy)
    )

let seqTapi action source =
    Seq.mapi (fun i v -> action i v; v) source

let tap f v = f v; v

let traverseAndCountTrees dx dy map =
    walkTilOutOfBounds dx dy map
    //|> seqTapi (fun i (pos, node) -> printfn " i=%-3i  left=%-3i  right=%-3i  node=%A" i pos.Left pos.Top node)
    |> Seq.map snd
    |> Seq.filter ((=) Tree)
    |> Seq.length

let traverseAndPrintResult dx dy map =
    printf "Traversing with right %i & down %i... " dx dy
    traverseAndCountTrees dx dy map
    |> tap (printfn "Came across %i trees")

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let jagged = lines |> Array.map parseLine
    let grid = jaggedTo2D jagged
    let map = { Grid = grid; Position = { Top = 0; Left = 0 } }

    printfn "Parsed %ix%i grid" (Array2D.length1 grid) (Array2D.length2 grid)

    [
        traverseAndPrintResult 1 1 map
        traverseAndPrintResult 3 1 map // part 1
        traverseAndPrintResult 5 1 map
        traverseAndPrintResult 7 1 map
        traverseAndPrintResult 1 2 map
    ] |> List.reduce (*)
    |> printfn "Product of all tree counts: %i"

    0

