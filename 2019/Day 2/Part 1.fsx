open System.IO
let input = File.ReadAllText("input.txt");

let program = input.Split(',') |> Array.map int

let add (program: int array) position =
    let value1 = program.[program.[position + 1]]
    let value2 = program.[program.[position + 2]]
    Array.set program program.[position + 3] (value1 + value2)
    program

let multiply (program: int array) position =
    let value1 = program.[program.[position + 1]]
    let value2 = program.[program.[position + 2]]
    Array.set program program.[position + 3] (value1 * value2)
    program

let rec parse (program: int array) position =
    match program.[position] with
    | 1 -> parse (add program position) (position + 4)
    | 2 -> parse (multiply program position) (position + 4)
    | 99 -> program
    | _ -> program


printfn "%A" (parse program 0).[0]

// 4090689