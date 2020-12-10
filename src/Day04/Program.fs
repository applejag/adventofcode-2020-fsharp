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

let validatePassport1 passport =
    passport.byr <> None
    && passport.iyr <> None
    && passport.eyr <> None
    && passport.hgt <> None
    && passport.hcl <> None
    && passport.ecl <> None
    && passport.pid <> None
    //&& passport.cid <> None

let colorRegex = Regex(@"^#[0-9a-f]{6}$", RegexOptions.Compiled)
let isValidColor = colorRegex.IsMatch

let isIntInRange min max i =
    i >= min && i <= max

let isValidIntInRange min max str =
    try
        isIntInRange min max (int str)
    with
    | _ -> false

let distanceRegex = Regex(@"^(\d+)(cm|in)$", RegexOptions.Compiled)
type Distance =
    | Centimeter of int
    | Inches of int

let isValidDistance str =
    let matchDistance str =
        let m = distanceRegex.Match str
        match m.Success with
        | true -> Some (int m.Groups.[1].Value, m.Groups.[2].Value)
        | false -> None

    match matchDistance str with
    | Some (i, "cm") -> isIntInRange 150 193 i
    | Some (i, "in") -> isIntInRange 59 76 i
    | _ -> false

let validEyeColors = Set.ofList [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
let isValidEyeColor str = Set.contains str validEyeColors

let pidRegex = Regex(@"^\d{9}$", RegexOptions.Compiled)
let isValidPid = pidRegex.IsMatch

let isSomeAnd f = function
    | None -> false
    | Some v -> f v

let validatePassport2 passport =
    passport.byr |> isSomeAnd (isValidIntInRange 1920 2002)
    && passport.iyr |> isSomeAnd (isValidIntInRange 2010 2020)
    && passport.eyr |> isSomeAnd (isValidIntInRange 2020 2030)
    && passport.hgt |> isSomeAnd isValidDistance
    && passport.hcl |> isSomeAnd isValidColor
    && passport.ecl |> isSomeAnd isValidEyeColor
    && passport.pid |> isSomeAnd isValidPid

module Array =
    module Parallel =
        let count predicate arr =
            Array.Parallel.map predicate arr
            |> Seq.filter ((=) true)
            |> Seq.length

[<EntryPoint>]
let main args =
    let filename = Array.tryItem 0 args |> Option.defaultValue "./input.txt"
    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let passportSections = splitPassportSections lines |> Array.ofList
    printfn "Found %i passports" passportSections.Length

    let passports =
        passportSections
        |> Array.Parallel.map parsePassportMapOfLines
        |> Array.Parallel.map parsePassportOfMap

    printfn ""
    printfn "Part 1"
    passports
    |> Array.Parallel.count validatePassport1
    |> printfn " Found %i valid passports"

    printfn ""
    printfn "Part 2"
    passports
    |> Array.Parallel.count validatePassport2
    |> printfn " Found %i valid passports"

    0
