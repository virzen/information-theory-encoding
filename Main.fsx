open System.IO
open System

let inputFilename = "116-binary-tree.pdf"
let binaryFilename = "binary.01"

type Dictionary = Map<byte, string>
type ReversedDictionary = Map<string, byte>


// bytes

let readFileAsBytes filename =
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


// binary

let readFileAsBinary filename =
  File.ReadAllText filename

let binaryToInt (b: System.String) =
  Convert.ToInt32(string b, 2)

let binaryOfWordLengthToInts wordLength (s: string): int[] = // custom format instead of string?
  s.ToCharArray()
    |> Array.chunkBySize wordLength
    |> Array.map System.String
    |> Array.map binaryToInt

let writeToFile filename s =
  File.WriteAllText(filename, s)


// distribution

type Distribution = Map<int, int>

let incrementCountOf map (value: int) =
  let maybePreviousOccurences = Map.tryFind value map

  let newOccurences =
    match maybePreviousOccurences with
      | Some x -> x + 1
      | None -> 1

  Map.add value newOccurences map

let distribution (values: int[]): Distribution =
    Array.fold incrementCountOf Map.empty values


// MAIN

inputFilename
  |> readFileAsBytes
  |> encode simpleDictionary
  |> writeToFile binaryFilename

binaryFilename
  |> readFileAsBinary
  |> binaryOfWordLengthToInts 8
  |> printf "%A"

binaryFilename
  |> readFileAsBinary
  |> binaryOfWordLengthToInts 8
  |> distribution
  |> printf "%A"

binaryFilename
  |> readFileAsBinary
  |> binaryOfWordLengthToInts 4
  |> distribution
  |> printf "%A"

binaryFilename
  |> readFileAsBinary
  |> binaryOfWordLengthToInts 1
  |> distribution
  |> printf "%A"
