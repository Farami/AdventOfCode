open System.IO


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


let rec program noun verb =
    let input = File.ReadAllText("input.txt").Split(',') |> Array.map int
    Array.set input 1 noun
    Array.set input 2 verb
    //printfn "%d %d" noun verb
    match (parse input 0).[0] with
    | 19690720 -> 100 * noun + verb
    | _ ->
        match verb with
        | x when x = 99 -> program (noun + 1) 0
        | _ -> program noun (verb + 1)

printfn "%d" (program 0 0)
// 4090689