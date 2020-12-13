open System.IO

let readLines filename =
    File.ReadAllLines filename

let splitBusIds (line: string) =
    line.Split ','
    |> Array.Parallel.map (function
        | "x" -> None
        | busId -> int busId |> Some
    )

let calcWaitingTimeForBus eta busId =
    busId - eta % busId

[<EntryPoint>]
let main argv =
    let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let eta = int lines.[0]
    let notes = splitBusIds lines.[1]
    let busIds = notes |> Array.filter Option.isSome |> Array.map Option.get

    printfn "Read %i notes, where %i are integers" notes.Length busIds.Length
    printfn "Estimated time of arrival after %i min" eta

    printfn ""
    printfn "Part 1:"
    let waitingTimes =
        busIds
        |> Array.Parallel.map (calcWaitingTimeForBus eta)
        |> Array.indexed
        |> Array.sortBy snd

    let shortestIndex, shortestTime = waitingTimes.[0]
    let shortestBusId = busIds.[shortestIndex]
    printfn " Next bus is bus #%i after %i min" shortestBusId shortestTime
    printfn " Product: %i" (shortestBusId*shortestTime)

    //printfn ""
    //printfn "Part 2:"

    0
