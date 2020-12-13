open System.IO

let readLines filename =
    File.ReadAllLines filename

[<EntryPoint>]
let main argv =
    let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    printfn ""
    printfn "Part 1:"

    //printfn ""
    //printfn "Part 2:"

    0
