open System.IO
let input = File.ReadAllText("input.txt");
let wires = input.Split('\n')

let firstWire = wires.[0]
let secondWire = wires.[1]

type Step =
    { Direction: char
      Length: int }

type Wire =
    { Steps: Step array }

exception InvalidDirectionException
let toPoints (wire: Wire) =
    let mutable currentPosition = (0, 0)
    seq {
        for step in wire.Steps do
            for _ in 0 .. (step.Length - 1) do
                let x, y = currentPosition
                currentPosition <-
                    match step.Direction with
                    | 'U' -> (x, y + 1)
                    | 'D' -> (x, y - 1)
                    | 'L' -> (x - 1, y)
                    | 'R' -> (x + 1, y)
                    | _ -> raise InvalidDirectionException
                yield currentPosition
    }

let parseStep (input: string) =
    { Direction = input.[0]
      Length = int (input.Substring 1) }

let parseWire (input: string) = { Steps = input.Split(',') |> Array.map parseStep }

let points = (parseWire firstWire |> toPoints, parseWire secondWire |> toPoints)
let intersections = Set.intersect (Set.ofSeq <| fst points) (Set.ofSeq <| snd points)
let manhattanDistances = Set.map (fun (x, y) -> abs x + abs y)

printfn "Smallest manhattan distance: %d" (Seq.min (manhattanDistances <| intersections))
