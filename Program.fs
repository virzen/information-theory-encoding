﻿open System.IO
open System



type Node =
    | Parent of Node * Node
    | Leaf of byte

type EncodingTree = Node

type Dictionary = Map<byte, string>

type ReversedDictionary = Map<string, byte>


module Dictionary = 
    let empty =
        Map.empty<byte, string>


    let ofEncodingTree (tree : EncodingTree) : Dictionary = 
        //let step visited stack dict node: Dictionary =
            //match node with 
            //| Parent left, right ->

        failwith "Not implemented"


    let reverse (dictionary : Dictionary) : ReversedDictionary =
        Map.fold (fun dict key value -> dict.Add(value, key)) Map.empty dictionary



let joinWith (separator : string) (iterable : seq<char>) = String.Join(separator, iterable)



type Distribution = Map<byte, int>

module Distribution =
  let incrementCountOf map (value : byte) =
      let maybePreviousOccurences = Map.tryFind value map

      let newOccurences =
          match maybePreviousOccurences with
          | Some x -> x + 1
          | None -> 1

      Map.add value newOccurences map

  let from (values : byte []) : Distribution = Array.fold incrementCountOf Map.empty values



// encoding

let readBytes filename = File.ReadAllBytes(filename)

let intToBinary (x : int) = Convert.ToString(x, 2)

let padWithZeros length (x : string) = x.PadLeft(length, '0')

let byteToBinaryOfWordLength wordLength =
    int
    >> intToBinary
    >> (padWithZeros wordLength)

let encode (dictionary : Map<byte, string>) (bytes : byte []) =
    bytes
    |> Array.map (fun byte -> Map.find byte dictionary)
    |> String.concat ""

let simpleDictionary =
    let bytes = seq { 0..255 } |> Seq.map byte
    let encoded = Seq.map (byteToBinaryOfWordLength 8) bytes
    let pairs = Seq.zip bytes encoded

    Map.ofSeq pairs




// decoding

let readText filename = File.ReadAllText filename

let binaryToByte (b : System.String) = Convert.ToInt32(string b, 2)

let findFirstSymbolFromDictionary (dictionary : ReversedDictionary) (symbols : char []) : byte * char [] =
    let mutable found = false
    let mutable result = 0 |> byte
    let mutable numOfDigits = 1

    while not found do
        let key = Array.take numOfDigits symbols |> joinWith ""
        match Map.tryFind key dictionary with
        | Some value ->
            do result <- value
               found <- true
        | None -> do numOfDigits <- numOfDigits + 1

    result, symbols.[numOfDigits..]

let decode (dictionary : ReversedDictionary) (s : string) : byte [] =
    let mutable chars : char [] = s.ToCharArray()
    let mutable complete = false
    let mutable result : byte [] = [||]

    while not complete do
        let byte, newChars = findFirstSymbolFromDictionary dictionary chars

        result <- Array.append result [| byte |]

        if Array.length newChars > 0 then do chars <- newChars
        else do complete <- true

    result


let writeStringTo filename s = File.WriteAllText(filename, s)

let writeBytesTo filename bytes = File.WriteAllBytes(filename, bytes)




let shannonFano (bytes : byte []) : EncodingTree =
    let sortedPairs =
        Distribution.from bytes
        |> Map.toArray
        |> Array.sortBy (fun (_byte, count) -> count)

    let sortedSigns = sortedPairs |> Array.map (fun (byte, _count) -> byte)
    
    let balancedSplit (signs : byte []) : byte [] * byte [] = 
        let mutable minDiff = System.Int32.MaxValue;
        let mutable index = -1;

        let ints = Array.map int signs
        
        for i = 0 to ((Array.length signs) - 2) do
  
            let leftSum = Array.sum ints.[0..i]
            let rightSum = Array.sum ints.[(i+1)..]
            let diff = abs (leftSum - rightSum)

            if diff < minDiff
            then do
                minDiff <- diff
                index <- i
                
        signs.[0..index], signs.[(index + 1)..]


    let rec nodify (signs : byte []) : Node =
        match signs with
        | [| x |] -> Leaf x
        | xs ->
            let left, right = balancedSplit signs
            Parent((nodify left), (nodify right))

    nodify sortedSigns

let huffman (bytes : byte []) : Dictionary = failwith "Not implemented"

[<EntryPoint>]
let main argv =
    let bytes = readBytes "../../../116-binary-tree.pdf"
    let encodingTree = shannonFano bytes
    let dictionary = Dictionary.ofEncodingTree encodingTree

    encode dictionary bytes |> writeStringTo "../../../Main.fsx.01"

    //readText "../../../Main.fsx.01"
    //|> decode (reverse simpleDictionary)
    //|> writeBytesTo "../../../Main.rebuild.fsx"

    0 // return an integer exit code