open System.IO
open System

let inputFilename = "116-binary-tree.pdf"
let binaryFilename = "binary.01"


// bytes

let readFileAsBytes filename =
  File.ReadAllBytes(filename)

let intToBinary (x: int) =
  Convert.ToString(x, 2)

let padWithZeros length (x: string) =
  x.PadLeft(length, '0')

let byteToBinaryOfWordLength wordLength =
  int >> intToBinary >> (padWithZeros wordLength)

let bytesToBinaryOfWordLength wordLength bytes =
  bytes
    |> Array.map (byteToBinaryOfWordLength wordLength)
    |> String.concat ""


// binary

let readFileAsBinary filename =
  File.ReadAllText filename

let binaryToInt b =
  Convert.ToInt32(b, 2)

let binaryOfWordLengthToInts wordLength (s: string) = // custom format instead of string?
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


[<EntryPoint>]
let main argv =
    inputFilename
      |> readFileAsBytes
      |> bytesToBinaryOfWordLength 8
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


    0 // return an integer exit code
