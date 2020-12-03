open System.IO

let map =
    File.ReadAllText("input.txt").Split '\n'
    |> Seq.map Seq.toList
    |> Seq.toList

let traverse xStep yStep (map: List<List<char>>) =
  let mapWidth = map.[0].Length
  let mapHeight = map.Length

  let getPoint x y = map.[y].[x % mapWidth]
  let isTree = (=) '#'

  let rec innerTraverse x y (sum: uint32) =
      match y with
      | y when y >= mapHeight - 1 -> sum
      | _ -> innerTraverse (x + xStep) (y + yStep) (sum + if isTree <| getPoint x y then 1u else 0u)
  innerTraverse 0 0 0u

Seq.reduce (*) [|traverse 1 1 map; traverse 3 1 map; traverse 5 1 map; traverse 7 1 map; traverse 1 2 map|]
|> printfn "%d"