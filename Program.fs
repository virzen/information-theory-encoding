open System.IO
open System
open FSharpx.Collections

type Node =
    | Parent of Node * Node
    | Leaf of byte

type EncodingTree = Node

type Dictionary = Map<byte, string>

type ReversedDictionary = Map<string, byte>



let joinSymbolsWith (separator : string) (iterable : seq<char>) = String.Join(separator, iterable)

let joinIntsWith (separator: string) (iterable : seq<int>) = 
    let strings = Seq.map string iterable

    String.Join(separator, strings)

let joinStringsWith (separator : string) (iterable : seq<string>) = String.Join(separator, iterable)


module Dictionary = 
    let empty =
        Map.empty<byte, string>
        
    let reverse (dictionary : Dictionary) : ReversedDictionary =
        Map.fold (fun dict key value -> dict.Add(value, key)) Map.empty dictionary

    let entries (d : Dictionary) =
        Map.toList d


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

let findFirstSymbolFromDictionary (dictionary : ReversedDictionary) (symbols : char list) : byte * (char list) =
    let mutable found = false
    let mutable result = 0 |> byte
    let mutable numOfDigits = 1

    let rec loop triedChars symbols =
        match symbols with 
        | char::rest ->
            let charsOfKey = PersistentVector.conj char triedChars
            let key = joinSymbolsWith "" charsOfKey

            match Map.tryFind key dictionary with
            | Some value ->
                value, rest
            | None -> 
                loop charsOfKey rest 
        | [] -> failwith "Cannot find symbol in dictionary"

    loop PersistentVector.empty symbols


let decode (dictionary : ReversedDictionary) (s : string) : byte [] =
    let charactersToProcess = String.length s

    printfn "Decoding string of length %d" charactersToProcess

    let findFirst = findFirstSymbolFromDictionary dictionary

    let rec loop chars result =
        let byte, newChars = findFirst chars
        let newResult = PersistentVector.conj byte result

        match newChars with 
        | [] -> newResult
        | xs -> 
            printfn "Decoding progress: %d" (PersistentVector.length result)
            loop newChars newResult

    let chars = s.ToCharArray() |> List.ofArray
    let result = PersistentVector.empty<byte>

    loop chars result |> Array.ofSeq

    
    //while not complete do
    //    let byte, newChars = findFirstSymbolFromDictionary dictionary chars

    //    result <- PersistentVector.conj byte result

    //    if List.length newChars > 0 then do chars <- newChars
    //    else do complete <- true

    //    printfn "Decoding progress: %d/%d" (PersistentVector.length result) (charactersToProcess)

    //result |> Array.ofSeq


let writeStringTo filename s = File.WriteAllText(filename, s)

let writeBytesTo filename bytes = File.WriteAllBytes(filename, bytes)


let shannonFano (bytes : byte []) : EncodingTree =
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

    let rec nodify (signs : byte []): EncodingTree =
        match signs with
        | [| x |] -> 
            Leaf x
        | xs ->
            let left, right = balancedSplit signs
            Parent((nodify left), (nodify right))

    let bytesWithCountsSorted = Distribution.from bytes
                                |> Map.toArray
                                |> Array.sortBy (fun (_byte, count) -> count)
                                |> Array.rev

    printfn "Bytes sorted by occurences:\n%A" bytesWithCountsSorted

    let bytesSorted = Array.map (fun (byte, _count) -> byte) bytesWithCountsSorted

    nodify bytesSorted



let huffman (bytes : byte []) : Dictionary = failwith "Not implemented"


let encodingTreeToDictionary (tree : EncodingTree) : Dictionary =
    let mapSecond f (a, b) =
        (a, f b)

    let bitsToString (bits : PersistentVector<int>) : string =
        joinIntsWith "" bits

    let rec loop bits node =
      match node with 
      | Leaf x -> 
          [(x, bits)]
      | Parent (left, right) ->
          let leftBits = PersistentVector.conj 0 bits
          let rightBits = PersistentVector.conj 1 bits
          (loop leftBits left) @ (loop rightBits right)

    loop PersistentVector.empty tree |> List.map (mapSecond bitsToString) |> Map.ofSeq

let dictCodewordLength ((_, codeword) : byte * string) : int =
    String.length codeword

[<EntryPoint>]
let main argv =
    let bytes = readBytes "../../../116-binary-tree.pdf"

    printfn "Creting encoding tree using Shannon-Fano method"

    let encodingTree = shannonFano bytes

    printfn "Creating dictionary from encoding tree"

    let dictionary = encodingTreeToDictionary encodingTree

    printfn "Dictionary created:\n%A" dictionary

    printfn "Dictionary entries by codeword length:\n%A" (Dictionary.entries dictionary |> List.sortBy dictCodewordLength)

    printfn "Encoding file with created dictionary"

    encode dictionary bytes |> writeStringTo "../../../116-binary-tree.01"

    printfn "Encoded, decoding"

    readText "../../../116-binary-tree.01"
    |> decode (Dictionary.reverse dictionary)
    |> writeBytesTo "../../../116-binary-tree.rebuilt.pdf"

    0 // return an integer exit code
