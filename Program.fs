open System.IO
open System


let joinWith (separator: string) (iterable: seq<char>) = String.Join(separator, iterable)


// distribution

type Distribution = Map<int, int>

let incrementCountOf map (value: int) =
  let maybePreviousOccurences = Map.tryFind value map

  let newOccurences =
    match maybePreviousOccurences with
      | Some x -> x + 1
      | None -> 1

  Map.add value newOccurences map

//let distribution (values: byte[]): Distribution =
    //Array.fold incrementCountOf Map.empty values


type Dictionary = Map<byte, string>
type ReversedDictionary = Map<string, byte>


// encoding

let readBytes filename =
  File.ReadAllBytes(filename)

let intToBinary (x: int) =
  Convert.ToString(x, 2)

let padWithZeros length (x: string) =
  x.PadLeft(length, '0')

let byteToBinaryOfWordLength wordLength =
  int >> intToBinary >> (padWithZeros wordLength)

let encode (dictionary: Map<byte, string>) (bytes: byte[]) =
  bytes
    |> Array.map (fun byte -> Map.find byte dictionary)
    |> String.concat ""


let simpleDictionary =
  let bytes = seq { 0 .. 255 } |> Seq.map byte
  let encoded = Seq.map (byteToBinaryOfWordLength 8) bytes
  let pairs = Seq.zip bytes encoded

  Map.ofSeq pairs


let reverse (dictionary: Dictionary): ReversedDictionary =
      Map.fold (fun dict key value -> dict.Add(value,key)) Map.empty dictionary


// decoding

let readText filename =
  File.ReadAllText filename

let binaryToByte (b: System.String) =
  Convert.ToInt32(string b, 2)

let findFirstSymbolFromDictionary (dictionary: ReversedDictionary) (symbols: char[]): byte * char[] =
  let mutable found = false
  let mutable result = 0 |> byte
  let mutable numOfDigits = 1

  while not found do
    let key = Array.take numOfDigits symbols |> joinWith ""
    match Map.tryFind key dictionary with
    | Some value -> do
      result <- value
      found <- true
    | None -> do
      numOfDigits <- numOfDigits + 1

  result, symbols.[numOfDigits..]

let decode (dictionary: ReversedDictionary) (s: string): byte[] =
  let mutable chars: char[] = s.ToCharArray()
  let mutable complete = false
  let mutable result: byte[] = [||]

  while not complete do
    let byte, newChars = findFirstSymbolFromDictionary dictionary chars

    result <- Array.append result [| byte |]

    if Array.length newChars > 0 then do
      chars <- newChars
    else do
      complete <- true

  result


let writeStringTo filename s =
  File.WriteAllText(filename, s)

let writeBytesTo filename bytes =
  File.WriteAllBytes(filename, bytes)



type Node = | TwoDescendants of Node * Node | OneDescendant of Node | Leaf of byte

type EncodingTree = Node

let dictOfEncodingTree (tree: EncodingTree): Dictionary =
  failwith "Not implemented"

//let buildEncodingTree (bytes: byte[]): EncodingTree =
  //let splitBalanced pairs =

  //let sortedPairs = distribution bytes |> Map.toSeq |> Seq.sortBy (fun (_byte, count) -> count)


let shannonFano (bytes: byte[]): Dictionary =
  failwith "Not implemented"



let huffman (bytes: byte[]): Dictionary =
  failwith "Not implemented"


[<EntryPoint>]
let main argv =
    readBytes "../../../Main.fsx"
      |> encode simpleDictionary
      |> writeStringTo "../../../Main.fsx.01"

    readText "../../../Main.fsx.01"
      |> decode (reverse simpleDictionary)
      |> writeBytesTo "../../../Main.rebuild.fsx"
      
    0 // return an integer exit code
