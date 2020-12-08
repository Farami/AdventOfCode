open System.IO

let input = File.ReadAllText("input.txt").Split('\n') |> Array.toList

let partition (seats: int list) step = 
  let partitionSize = seats.Length / 2
  let partitions = List.splitAt partitionSize seats
  match step with
  | 'F' | 'L' -> fst partitions
  | 'B' | 'R' -> snd partitions
  | _ ->  []

let rec parse (seats: int list) (stepSequence: char list) =
  match stepSequence with
  | head::tail -> parse (partition seats head) tail 
  | [] -> seats.Head 

let parseRow stepSequence =
  stepSequence
  |> List.take 7
  |> parse [0..128]

let parseColumn stepSequence =
  stepSequence
  |> List.skip 7
  |> parse [0..7]

let calculateId row column = row * 8 + column

let list = input 
            |> List.map ((fun x -> x.ToCharArray() |> Array.toList) >> (fun x -> calculateId (parseRow x) (parseColumn x))) 

let has x = List.contains x list

[List.min list..List.max list]
|> List.find (fun x -> not (has x) && has (x - 1) && has (x + 1))
|> printfn "%d"