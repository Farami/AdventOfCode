open System.IO
open System
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open System.Reflection

[<AbstractClass>]
type Validation() = 
  inherit Attribute()
  abstract member IsValid: string -> bool

type Required() = 
  inherit Validation()
  override __.IsValid(value) = value <> ""

type Record = IDictionary<string, obj>
type Passport = {
  [<Required>]byr: string;
  [<Required>]iyr: string;
  [<Required>]eyr: string;
  [<Required>]hgt: string;
  [<Required>]hcl: string;
  [<Required>]ecl: string;
  [<Required>]pid: string;
  cid: string;
} 

let HasAttribute (pi: PropertyInfo, attr) = Attribute.IsDefined(pi, attr)
let GetAttributes (pi: PropertyInfo) = 
  Attribute.GetCustomAttributes(pi) 
  |> Seq.filter (fun attr -> attr :? Validation) 
  |> Seq.map (fun e -> e :?> Validation)

let IsValid (passport: Passport) =
  let validatorResult (pi: PropertyInfo) =
    match HasAttribute(pi, typeof<Validation>) with
    | true -> GetAttributes(pi)
              |> Seq.map (fun attr -> attr.IsValid(pi.GetValue(passport) :?> string))
              |> Seq.forall ((=) true)
    | false -> true

  FSharpType.GetRecordFields(typeof<Passport>)
  |> Seq.map validatorResult
  |> Seq.forall ((=) true)

let extract<'T>(gd: IDictionary<string,obj>) = 
  let flds = FSharpType.GetRecordFields(typeof<'T>)
  let vals = [| for f in flds -> if gd.ContainsKey(f.Name) then gd.[f.Name] else box "" |]
  FSharpValue.MakeRecord(typeof<'T>, vals) :?> 'T


let parseKeyValue (value: string) = 
  match value.Split(':') with
  | [|key; value|] -> Some (key, box value)
  | _ -> None

let parseRecord (value: string) =
  value.Replace('\n', ' ').Split ' '
  |> Array.map parseKeyValue
  |> Array.choose id
  |> dict

let parsePassport (record: Record) = extract<Passport> record

File.ReadAllText("input.txt").Split([|"\n\n"|], StringSplitOptions.None)
  |> Array.map (parseRecord >> parsePassport >> IsValid)
  |> Array.filter ((=) true) 
  |> Array.length
  |> printfn "%A"