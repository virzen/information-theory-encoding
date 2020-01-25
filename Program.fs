open System.IO
open System
open FSharpx.Collections

type Codeword = string

type Node =
    | Parent of Node * Node
    | Leaf of byte * Codeword

type EncodingTree = Node

type Dictionary = Map<byte, string>

type ReversedDictionary = Map<string, byte>



let joinSymbolsWith (separator : string) (iterable : seq<char>) = String.Join(separator, iterable)

let joinIntsWith (separator: string) (iterable : seq<int>) = 
    let strings = Seq.map string iterable

    String.Join(separator, strings)


module Stack =
    type T<'a> = 'a list

    let empty: T<'a> = List.empty<'a>

    let push (stack: T<'a>) (x: 'a): T<'a> =
        x :: stack

    let pop (stack: T<'a>): 'a * T<'a> =
        match stack with
        | [] -> failwith "pop used on empty stack"
        | h::t -> h, t

    let values (stack: T<'a>): 'a list =
        List.rev stack 


module Dictionary = 
    let empty =
        Map.empty<byte, string>

    let add = Map.add

    let ofEncodingTree (tree : EncodingTree) : Dictionary = 
        let mutable dict = Dictionary.empty

        let rec step visited node =
            match node with 
            | Leaf value, codeword -> do
                dict <- Map.add codeword value

        failwith "Not implemented"


    let reverse (dictionary : Dictionary) : ReversedDictionary =
        Map.fold (fun dict key value -> dict.Add(value, key)) Map.empty dictionary





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
        let key = Array.take numOfDigits symbols |> joinSymbolsWith ""
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

    let rec nodify (bits) (signs : byte []) : Node =
        match signs with
        | [| x |] -> 
            let codeword: Codeword = joinIntsWith "" bits
            Leaf (x, codeword)
        | xs ->
            let left, right = balancedSplit signs
            let leftBits = PersistentVector.conj 0 bits
            let rightBits = PersistentVector.conj 1 bits
            Parent ((nodify leftBits left), (nodify rightBits right))

    Distribution.from bytes
    |> Map.toArray
    |> Array.sortBy (fun (_byte, count) -> count)
    |> Array.map (fun (byte, _count) -> byte)
    |> nodify PersistentVector.empty

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
