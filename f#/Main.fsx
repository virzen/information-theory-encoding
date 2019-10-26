open System.IO
open System

let inputFilename = "116-binary-tree.pdf"
let binaryFilename = "binary.01"

let intToBinary (x: int) =
  Convert.ToString(x, 2)

let padWithZeros length (x: string) =
  x.PadLeft(length, '0')

let byteToBinary =
  int >> intToBinary >> (padWithZeros 8)

let parseFileToBinary filename =
  filename
    |> File.ReadAllBytes
    |> Array.map byteToBinary
    |> String.concat ""

let writeToFile filename s =
  File.WriteAllText(filename, s)

let binaryToInt b =
  Convert.ToInt32(b, 2)

let parseBinaryFileToInts binaryLength filename =
  filename
  |> File.ReadAllText
  |> fun s -> s.ToCharArray()
  |> Array.chunkBySize binaryLength
  |> Array.map System.String
  |> Array.map binaryToInt



inputFilename
  |> parseFileToBinary
  |> writeToFile binaryFilename

binaryFilename
  |> parseBinaryFileToInts 8
  |> printf "%A"

// TODO: accumulate results in a map
