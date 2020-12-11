# Advent of Code 2020 via F#

This repo contains my attempt at Advent of Code 2020
(<https://adventofcode.com/2020>).

## Running it

- Install .NET 5.0

- Run with the `dotnet` CLI inside the project you wish to run.
  For day 1, you would run:

  ```sh
  cd src/Day01

  # To run it on "input.txt" in current working directory
  dotnet run

  # To run it on some other file, example:
  dotnet run -- sample.txt
  ```

## Adding a new puzzle solution

There is a template project, named `DayXX`. To make a new one, you may first
set the following environment variable:

```sh
AOC_DAY=03
```

Then run the following commands:

```sh
cp -r "src/DayXX" "src/Day$AOC_DAY"
mv "src/Day$AOC_DAY/DayXX.fsproj" "src/Day$AOC_DAY/Day$AOC_DAY.fsproj"
dotnet sln add "src/Day$AOC_DAY"
```

