open System.IO 

let seq = File.ReadAllText("input.txt").Split '\n' |> Seq.map int

seq 
|> Seq.filter (fun x -> Seq.exists (fun y -> x + y = 2020) <| seq)
|> Seq.fold (*) 1
|> printfn "%i"