open System.IO

let readLines filename =
    File.ReadAllLines filename

// Taken from Day03/Program.fs
let jaggedTo2D (jagged: 'a[][]) =
    let w = jagged.[0].Length
    let h = jagged.Length
    let init x y = jagged.[y].[x]

    Array2D.init w h init

let convertLinesTo2DArr lines =
    Array.map Array.ofSeq lines
    |> jaggedTo2D

let adjasentIndices = [|
    (-1,-1); (0,-1); (1,-1);
    (-1,0);  (*0,0*) (1,0);
    (-1,1);  (0,1);  (1,1);
|]

module Array2D =
    let tryItem index1 index2 arr =
        let length1 = Array2D.length1 arr
        let length2 = Array2D.length2 arr
        if index1 < 0 || index2 < 0 || index1 >= length1 || index2 >= length2 then
            None
        else
            Some arr.[index1, index2]

    let toSeq source =
        let length1 = Array2D.length1 source
        let length2 = Array2D.length2 source
        seq {
            for x in 0..length1-1 do
                for y in 0..length2-1 do
                    yield source.[x,y]
        }

let countAdjasentSeats c x y (grid: char[,]) =
    adjasentIndices
    |> Seq.map (fun (dx, dy) -> Array2D.tryItem (x+dx) (y+dy) grid)
    |> Seq.filter ((=) (Some c))
    |> Seq.length

let countSeats c (grid: char[,]) =
    Array2D.toSeq grid
    |> Seq.filter ((=) c)
    |> Seq.length

let evolveGrid grid =
    grid
    |> Array2D.mapi (fun x y c ->
        match c with
        | 'L' when countAdjasentSeats '#' x y grid = 0 -> '#'
        | '#' when countAdjasentSeats '#' x y grid >= 4 -> 'L'
        | _ -> c
    )

//let printGrid grid =
//    let w = Array2D.length1 grid
//    let h = Array2D.length2 grid
//    printfn "%s" (System.String('-', w))
//    for y in 0..h-1 do
//        for x in 0..w-1 do
//            printf "%c" grid.[x,y]
//        printfn ""
//    printfn "%s" (System.String('-', w))

[<EntryPoint>]
let main args =
    let filename = Array.tryItem 0 args |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let grid = convertLinesTo2DArr lines
    printfn "Read %ix%i grid" (Array2D.length1 grid) (Array2D.length2 grid)

    printfn ""
    Seq.unfold (fun (prevOccupied, g) ->
        let occupied = countSeats '#' g
        if occupied = prevOccupied then
            None
        else
            let nextGrid = evolveGrid g
            Some (occupied, (occupied, nextGrid))
    ) (-1, grid)
    |> Seq.iteri (fun i occupied ->
        printfn "Iteration %3i: %5i seats occupied" i occupied
    )

    0
