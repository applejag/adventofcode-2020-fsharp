open System.IO

type Node =
    | Tree
    | Empty

type Map = Node[,]

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

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let jagged = lines |> Array.map parseLine
    let grid = jaggedTo2D jagged
    printfn "Parsed %ix%i grid" (Array2D.length1 grid) (Array2D.length2 grid)

    0
