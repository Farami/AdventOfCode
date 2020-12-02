open System.IO 

let seq = File.ReadAllText("input.txt").Split '\n' |> Seq.map int

seq 
|> Seq.filter (fun x -> Seq.exists (fun y -> Seq.exists (fun z ->  x + y + z = 2020) seq) seq)
|> Seq.fold (*) 1
|> printfn "%i"