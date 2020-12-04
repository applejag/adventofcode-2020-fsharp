open System.IO
open System.Text.RegularExpressions

type Passport = {
    byr: string option // Birth Year
    iyr: string option // Issue Year
    eyr: string option // Expiration Year
    hgt: string option // Height
    hcl: string option // Hair Color
    ecl: string option // Eye Color
    pid: string option // Passport ID
    cid: string option // Country ID
}

let readLines filename = File.ReadAllLines filename

let splitPassportSections lines =
    Array.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail
    ) [[]] lines

let passportKeyValueRegex = Regex(@"(\w+):([^ ]+)", RegexOptions.Compiled)

let parsePassportMapOfLines lines =
    lines
    |> Seq.collect passportKeyValueRegex.Matches
    |> Seq.map (fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
    |> Map.ofSeq

let parsePassportOfMap (map: Map<string,string>) =
    let tryFind key = Map.tryFind key map
    {
        byr = tryFind "byr"
        iyr = tryFind "iyr"
        eyr = tryFind "eyr"
        hgt = tryFind "hgt"
        hcl = tryFind "hcl"
        ecl = tryFind "ecl"
        pid = tryFind "pid"
        cid = tryFind "cid"
    }

let validatePassport passport =
    passport.byr <> None
    && passport.iyr <> None
    && passport.eyr <> None
    && passport.hgt <> None
    && passport.hcl <> None
    && passport.ecl <> None
    && passport.pid <> None
    //&& passport.cid <> None

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let passportSections = splitPassportSections lines |> Array.ofList
    printfn "Found %i passports" passportSections.Length

    let passports =
        passportSections
        |> Array.Parallel.map parsePassportMapOfLines
        |> Array.Parallel.map parsePassportOfMap

    let validPassports =
        passports
        |> Array.filter validatePassport

    printfn "Found %i valid passports" validPassports.Length

    0
