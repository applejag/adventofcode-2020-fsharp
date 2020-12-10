open System.IO

let readLines filename =
    File.ReadAllLines filename

[<EntryPoint>]
let main argv =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    0
