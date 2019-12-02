open System.IO

let calcMass input =
  let rec calcModule x =
    let result = int <| floor(float x / 3.0) - 2.0
    match result with 
    | x when x > 0 -> result + (calcModule result)
    | _ -> 0
  Seq.sumBy calcModule input

let input = File.ReadAllText("input.txt").Split '\n' |> Array.map int

printfn "%d" (calcMass input)

