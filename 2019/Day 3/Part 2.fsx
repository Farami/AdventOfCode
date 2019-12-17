open System.IO

let input = File.ReadAllText("input.txt")
let wires = input.Split('\n')

let firstWire = wires.[0]
let secondWire = wires.[1]

[<CustomEquality; CustomComparison>]
type Point =
    { x: int
      y: int
      length: int }

    override this.Equals(that) =
        match that with
        | :? Point as point -> (this.x, this.y) = (point.x, point.y)
        | _ -> false

    override this.GetHashCode() = (this.x, this.y).GetHashCode()
    interface System.IComparable with
        member this.CompareTo that =
            match that with
            | :? Point as point -> compare (this.x, this.y) (point.x, point.y)
            | _ -> -1

type Step =
    { Direction: char
      Length: int }

type Wire =
    { Steps: Step array }

exception InvalidDirectionException

let toPoints (wire: Wire) =
    let mutable currentPosition =
        { x = 0
          y = 0
          length = 0 }
    seq {
        for step in wire.Steps do
            for _ in 0 .. (step.Length - 1) do
                currentPosition <-
                    match step.Direction with
                    | 'U' ->
                        { currentPosition with
                              y = currentPosition.y + 1
                              length = currentPosition.length + 1 }
                    | 'D' ->
                        { currentPosition with
                              y = currentPosition.y - 1
                              length = currentPosition.length + 1 }
                    | 'L' ->
                        { currentPosition with
                              x = currentPosition.x - 1
                              length = currentPosition.length + 1 }
                    | 'R' ->
                        { currentPosition with
                              x = currentPosition.x + 1
                              length = currentPosition.length + 1 }
                    | _ -> raise InvalidDirectionException
                yield currentPosition
    }

let parseStep (input: string) =
    { Direction = input.[0]
      Length = int (input.Substring 1) }

let parseWire (input: string) = { Steps = input.Split(',') |> Array.map parseStep }

let toPointsList =
    parseWire
    >> toPoints
    >> Seq.toList

let pointsA = toPointsList firstWire
let pointsB = toPointsList secondWire

let intersections = Set.intersect (Set.ofList pointsA) (Set.ofList pointsB) |> Set.toList

let getPoint a = List.find (fun b -> a = b)
let running point = point.length

let wireLengths = Seq.map (fun x -> (getPoint x pointsA |> running) + (getPoint x pointsB |> running))

printfn "Length: %d" (Seq.min <| wireLengths intersections)