open System.IO
open System
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Text.RegularExpressions

[<AbstractClass>]
type Validation() = 
  inherit Attribute()
  abstract member IsValid: string -> bool

type Required() = 
  inherit Validation()
  override __.IsValid(value) = value <> ""

type Between(min, max) =
  inherit Validation()
  override __.IsValid(value) = 
    match System.Int32.TryParse value with 
    | true,parsed -> parsed >= min && parsed <= max
    | _ -> false

type Length(min, max) =
  inherit Validation()
  override __.IsValid(value) =
    value.Length >= min && value.Length <= max

type ValidHeight() =
  inherit Validation()
  override __.IsValid(value) =
    let height = Regex.Replace(value, "[^0-9]", "") |> int
    match value with
      | value when value.Contains("cm") -> height >= 150 && height <= 193
      | value when value.Contains("in") -> height >= 59 && height <= 76
      | _ -> false

type ValidRegex(regex) =
  inherit Validation()
  override __.IsValid(value) =
    Regex.Match(value, regex).Success

type OneOf(arr) =
  inherit Validation()
  override __.IsValid(value) =
    Array.exists ((=) value) arr

type Record = IDictionary<string, obj>
type Passport = {
  [<Required>][<Between(1920, 2002)>]byr: string;
  [<Required>][<Between(2010, 2020)>]iyr: string;
  [<Required>][<Between(2020, 2030)>]eyr: string;
  [<Required>][<ValidHeight>]hgt: string;
  [<Required>][<ValidRegex("#(([a-z]|[0-9]){6})")>]hcl: string;
  [<Required>][<OneOf([|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|])>]ecl: string;
  [<Required>][<Length(9, 9)>]pid: string;
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