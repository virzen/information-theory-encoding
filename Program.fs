open System.IO
open System
open FSharpx.Collections

type Sign = byte

type Codeword = string

type Node =
    | Parent of Node * Node
    | Leaf of Sign

type EncodingTree = Node

type Dictionary = Map<Sign, Codeword>

type ReverseDictionary = Map<Codeword, Sign>



let joinSymbolsWith (separator : string) (iterable : seq<char>) = String.Join(separator, iterable)

let joinIntsWith (separator: string) (iterable : seq<int>) = 
    let strings = Seq.map string iterable

    String.Join(separator, strings)

let joinStringsWith (separator : string) (iterable : seq<string>) = String.Join(separator, iterable)


let (./.) x y =
    ((x |> float) / (y |> float))


module Dictionary = 
    let empty =
        Map.empty<Sign, Codeword>
        
    let reverse (dictionary : Dictionary) : ReverseDictionary =
        Map.fold (fun dict key value -> dict.Add(value, key)) Map.empty dictionary

    let entries (d : Dictionary) =
        Map.toList d


type Distribution = Map<Sign, int>

module Distribution =
  let incrementCountOf map (value : Sign) =
      let maybePreviousOccurences = Map.tryFind value map

      let newOccurences =
          match maybePreviousOccurences with
          | Some x -> x + 1
          | None -> 1

      Map.add value newOccurences map

  let from (values : Sign []) : Distribution = Array.fold incrementCountOf Map.empty values



// encoding

let readBytes filename = File.ReadAllBytes(filename)

let intToBinary (x : int) = Convert.ToString(x, 2)

let padWithZeros length (x : string) = x.PadLeft(length, '0')

let byteToBinaryOfWordLength wordLength =
    int
    >> intToBinary
    >> (padWithZeros wordLength)

let encode (dictionary : Dictionary) (signs : Sign []) =
    signs
    |> Array.map (fun sign -> Map.find sign dictionary)
    |> String.concat ""

let simpleDictionary : Dictionary =
    let signs : seq<Sign> = seq { 0..255 } |> Seq.map byte
    let encoded = Seq.map (byteToBinaryOfWordLength 8) signs
    let pairs = Seq.zip signs encoded

    Map.ofSeq pairs




// decoding

let readText filename = File.ReadAllText filename

let binaryToByte (b : System.String) = Convert.ToInt32(string b, 2)

let findFirstSymbolFromDictionary (dictionary : ReverseDictionary) (characters : char list) : Sign * (char list) =
    let mutable found = false
    let mutable result = 0 |> byte
    let mutable numOfDigits = 1

    let rec loop triedChars symbols : Sign * char list =
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

    loop PersistentVector.empty characters


let decode (dictionary : ReverseDictionary) (text : string) : Sign [] =
    let charactersToProcess = String.length text

    printfn "Decoding string of length %d" charactersToProcess

    let firstMatchingSign = findFirstSymbolFromDictionary dictionary

    let rec loop chars result =
        let sign, newChars = firstMatchingSign chars
        let newResult = PersistentVector.conj sign result

        match newChars with 
        | [] -> newResult
        | xs -> loop newChars newResult

    let chars = text.ToCharArray() |> List.ofArray
    let result = PersistentVector.empty<byte>

    loop chars result |> Array.ofSeq


let writeStringTo filename s = File.WriteAllText(filename, s)

let writeBytesTo filename bytes = File.WriteAllBytes(filename, bytes)


let shannonFano (bytes : byte []) : EncodingTree =
    let mapFirst (f : 'a -> 'c) ((a, b) : 'a * 'b) : 'c * 'b =
        (f a, b)

    let second ((a, b) : 'a * 'b) : 'b =
        b

    let balancedSplit (signsWithCounts : (Sign * int) []) : (Sign * int) [] * (Sign * int) [] = 
        let mutable minDiff = System.Int32.MaxValue;
        let mutable index = -1;

        let ints = Array.map (mapFirst int) signsWithCounts

        for i = 0 to ((Array.length signsWithCounts) - 2) do
            let leftSum = Array.sumBy second ints.[0..i]
            let rightSum = Array.sumBy second ints.[(i+1)..]
            let diff = abs (leftSum - rightSum)

            if diff < minDiff
            then do
                minDiff <- diff
                index <- i

        signsWithCounts.[0..index], signsWithCounts.[(index + 1)..]

    let rec nodify (signsWithCounts : (byte * int) []): EncodingTree =
        match signsWithCounts with
        | [| x |] -> 
            let sign, _count = x
            Leaf sign
        | xs ->
            let left, right = balancedSplit signsWithCounts
            Parent((nodify left), (nodify right))

    let bytesWithCountsSorted = Distribution.from bytes
                                |> Map.toArray
                                |> Array.sortBy (fun (_byte, count) -> count)
                                |> Array.rev

    printfn "Bytes sorted by occurences:\n%A" bytesWithCountsSorted

    nodify bytesWithCountsSorted



let huffman (signs : Sign []) : EncodingTree = failwith "Not implemented"


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

let duration f =
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    timer.Stop()
    let time = timer.ElapsedMilliseconds

    returnValue, time


let compressionRate (inputSigns : Sign []) (encodedString : string) : float = 
    (Array.length inputSigns) ./. (String.length encodedString)

//let programArgs argv =
    //let elementsAbove (xs : 'a array) (i : int) : 'a array =
    //    xs.[(i + 1)..]
    //let separatorIndex = Array.tryFindIndex ((=) "==") argv
    //Option.map (elementsAbove argv) separatorIndex


let exampleFile = "116-binary-tree.pdf"


let fileName (path : string) =
    Path.GetFileName(path)

let mapExtension f (path : string) =
    let oldExt = Path.GetExtension(path)
    Path.ChangeExtension(path, f(oldExt))

let fileNameToShannonFano (path : string) =
    mapExtension (fun old -> old + ".sf.01") path

let fileNameToRebuilt (path : string) =
    mapExtension (fun old -> ".rebuilt" + old) path

let fileNameToSimple (path : string) =
    mapExtension (fun old -> old + ".simple.01") path


let encodeDecode filePath =
    printfn "Processing file %s" (fileName filePath)
    printfn "==="

    let input = readBytes filePath

    printfn "Creating encoding tree using Shannon-Fano method"

    let encodingTree = shannonFano input

    printfn "Encoding tree created:\n%A" encodingTree
    printfn "Creating dictionary from encoding tree"

    let dictionary = encodingTreeToDictionary encodingTree

    printfn "Dictionary created:\n%A" dictionary
    printfn "Dictionary entries by codeword length:\n%A" (Dictionary.entries dictionary |> List.sortBy dictCodewordLength)

    printfn "Encoding with Shannon-Fano dictionary"
    let encodedInput = encode dictionary input

    printfn "Encoding with simple dictionary"
    let simplyEncodedInput = encode simpleDictionary input

    printfn "Shannon-Fano compression rate: %f" (compressionRate input encodedInput)
    printfn "Simple encoding compression rate: %f" (compressionRate input simplyEncodedInput)

    printfn "Saving to file"
    writeStringTo (fileNameToShannonFano filePath) encodedInput
    writeStringTo (fileNameToSimple filePath) simplyEncodedInput

    printfn "Decoding"

    let decodedInput, time = duration (fun _ -> decode (Dictionary.reverse dictionary) encodedInput)

    printfn "Decoded in %d ms" time

    writeBytesTo (fileNameToRebuilt filePath) decodedInput

    printf "\n"


[<EntryPoint>]
let main argv =
    Array.iter encodeDecode argv

    0 // return an integer exit code
