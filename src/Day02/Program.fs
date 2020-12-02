// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

type Password = string

type Policy = {
    Char: char
    Min: int
    Max: int
}

let fallsWithinRange min max num =
    num >= min && num <= max

type PolicydPassword = {
    Policy: Policy
    Password: Password
}

let pwdRegex = Regex(@"(\d+)-(\d+) (\w): (.*)", RegexOptions.Compiled)

let parsePassword line =
    let m = pwdRegex.Match line
    if m.Success then
        let pwd = m.Groups.[4].Value
        let policy = {
            Char = m.Groups.[3].Value.[0]
            Min = int m.Groups.[1].Value
            Max = int m.Groups.[2].Value
        }
        Some <| { Policy = policy; Password = pwd }
    else
        None

let readLines filename =
    File.ReadAllLines filename

let printResult (result: PolicydPassword[]) validator =
    result
    |> Array.Parallel.map (fun p -> validator p.Policy p.Password)
    |> Seq.filter id
    |> Seq.length
    |> printfn " Found %i valid passwords"

let validatePassword1 policy (pwd: Password) =
    pwd
    |> Seq.filter ((=) policy.Char)
    |> Seq.length
    |> fallsWithinRange policy.Min policy.Max

let xor a b = (a || b) && not (a && b)

let validatePassword2 policy (pwd: Password) =
    let pwdArr = pwd.ToCharArray()
    let hasCharAt pos =
        // Doing -1 because Min & Max references first as 1
        Array.tryItem (pos - 1) pwdArr = Some policy.Char

    xor (hasCharAt policy.Min) (hasCharAt policy.Max)

[<EntryPoint>]
let main _ =
    let pwds = readLines "./input.txt" |> Array.Parallel.choose parsePassword
    printfn "Found %i passwords" pwds.Length

    printfn ""
    printfn "Part 1:"
    printResult pwds validatePassword1

    printfn ""
    printfn "Part 2:"
    printResult pwds validatePassword2

    0 // return an integer exit code
