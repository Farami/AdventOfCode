let range = [| 137683 .. 596253 |]

let toDigitArray (number: int) = number.ToString().ToCharArray() |> Array.map (fun x -> int x - int '0')

let pairwiseFilter filter = 
  Array.pairwise
  >> Array.exists filter

let hasAdjacentSameDigits =
    toDigitArray
    >> pairwiseFilter (fun x -> fst x = snd x)

let hasNoDecreasingDigits =
    toDigitArray
    >> pairwiseFilter (fun x -> fst x > snd x)
    >> not

let possiblePasswords = 
  Array.filter hasAdjacentSameDigits 
  >> Array.filter hasNoDecreasingDigits

printfn "%A" (Array.length (possiblePasswords range))