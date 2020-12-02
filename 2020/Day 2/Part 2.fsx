open System.IO 

type PasswordPolicy = {
  FirstSpot: int
  SecondSpot: int
  Char: char
}

let split (separator: char) (input: string) = input.Split separator
let xor a b = a <> b

let splitColon = split ':'
let splitSpace = split ' '
let splitDash = split '-'

let first (input: string[]) = input.[0]
let second (input: string[]) = input.[1]

let parsePolicy (row: string) =
  let policyParts = row |> splitColon |> first |> splitSpace
  let x = policyParts |> first |> splitDash
  { FirstSpot = x |> first |> int; 
    SecondSpot = x |> second |> int; 
    Char = policyParts |> second |> char;
  }

let exists number = Seq.exists (fun x -> x = number)

let hasValidSpot policy spots = 
  xor (exists policy.FirstSpot spots) (exists policy.SecondSpot spots)

let isValid policy password = 
  password 
    |> Seq.indexed 
    |> Seq.filter (fun (_, x) -> x = policy.Char) 
    |> Seq.map fst
    |> hasValidSpot policy

File.ReadAllText("input.txt").Split '\n'
  |> Seq.filter (fun x -> splitColon x |> second |> isValid (parsePolicy x))
  |> Seq.length
  |> printfn "%d"