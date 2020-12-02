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

let validatePassword policy (pwd: Password) =
    pwd
    |> Seq.filter ((=) policy.Char)
    |> Seq.length
    |> fallsWithinRange policy.Min policy.Max

type PolicydPassword(policy, password) =
    member _.IsValid: bool = validatePassword policy password

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
        Some <| PolicydPassword(policy, pwd)
    else
        None

let readPasswords filename =
    File.ReadAllLines filename
    |> Array.Parallel.choose parsePassword

[<EntryPoint>]
let main argv =
    let pwds = readPasswords "./input.txt"
    printfn "Found %i passwords" pwds.Length
    printfn "Found %i valid passwords" (pwds |> Seq.filter (fun pwd -> pwd.IsValid) |> Seq.length)
    0 // return an integer exit code
