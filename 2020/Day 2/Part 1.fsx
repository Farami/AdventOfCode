open System.IO 

type PasswordPolicy = {
  Min: int
  Max: int
  Char: char
}

let parse (row: string) =
  let policyParts = (row.Split ':').[0].Split ' '
  let minMax = policyParts.[0].Split('-')
  { Min = minMax.[0] |> int; 
    Max = minMax.[1] |> int; 
    Char = policyParts.[1] |> char;
  }

let isValidCount policy charCount = 
  charCount >= policy.Min && charCount <= policy.Max

let isValid policy password = 
  password 
  |> Seq.filter (fun x -> x = policy.Char) 
  |> Seq.length
  |> isValidCount policy

File.ReadAllText("input.txt").Split '\n'
  |> Seq.filter (fun x -> isValid (parse x) (x.Split ':').[1])
  |> Seq.length
  |> printfn "%d"