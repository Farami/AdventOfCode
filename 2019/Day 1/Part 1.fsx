open System.IO

File.ReadAllText("input.txt").Split '\n'
|> Seq.sumBy (fun x -> floor(float x / 3.0) - 2.0)
|> int
|> printfn "%d"