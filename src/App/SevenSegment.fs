module SevenSegment

// type Digits = {
//     Zero: string;
//     One: string;
//     Two: string;
//     Three: string;
//     Four: string;
//     Five: string;
//     Six: string;
//     Seven: string;
//     Eight: string;
//     Nine: string
// }

// let mapStringToDigit s =
//     // printfn "%s" s
//     match s with
//     | "bd" -> 1 
//     | "abd" -> 7
//     | "aefg" -> 4
//     | "abcdefg" -> 8
//     | "bcdef" -> 5
//     | "acdfg" -> 2
//     | "acdfh" -> 3
//     | "abcdef" -> 9
//     | "bcdefg" -> 6
//     | "abcdeg" -> 0
//     | _ -> 1

// let countKnownDigits (input:seq<string>) =
//     input
//     |> Seq.map (fun i -> i.Split(" | "))
//     |> Seq.map (fun i -> i.[1])
//     |> Seq.map (fun i -> i.Split(" "))
//     |> Seq.map (fun i -> i |> Array.filter (fun s -> s.Length = 2 || s.Length = 3 || s.Length = 4 || s.Length = 7) |> Array.length)
//     |> Seq.sum

// let sortStr (s:string) =
//     let sortedStr = new System.String(s.ToCharArray() |> Array.sort)
//     printfn "sortedStr %s" sortedStr
//     sortedStr

// let codesToInt (codes:array<string>) =
//     // printfn "codes %A" codes
//     codes
//     |> Array.map (fun s -> sortStr s)
//     |> Array.map mapStringToDigit 
//     |> Array.map string
//     |> Array.reduce (+)

// let calculateDigitTotals (input:seq<string>) =
//     input
//     |> Seq.map (fun i -> i.Split(" | "))
//     |> Seq.map (fun i -> i.[1])
//     |> Seq.map (fun i -> i.Split(" "))
//     |> Seq.map (fun i -> codesToInt i)
//     // |> Seq.map int
//     // |> Seq.sum

// let rec firstPass (entry:list<string>) (digits:array<string>*array<string>*array<string>) =
//     match entry with
//     | h::t -> 
//         match h.Length with
//         | 2 -> firstPass t ([|digits.[0];h;digits.[2];digits.[3];digits.[4];digits.[5];digits.[6];digits.[7];digits.[8];digits.[9];|], 
//         | 3 -> firstPass t ([|digits.[0];digits.[1];digits.[2];digits.[3];digits.[4];digits.[5];digits.[6];h;digits.[8];digits.[9];|]
//         | 4 -> firstPass t ([|digits.[0];digits.[1];digits.[2];digits.[3];h;digits.[5];digits.[6];digits.[7];digits.[8];digits.[9];|]
//         | 7 -> firstPass t ([|digits.[0];digits.[1];digits.[2];digits.[3];digits.[4];digits.[5];digits.[6];digits.[7];h;digits.[9];|]
//         | _ -> firstPass t digits
//     | _ -> digits

let concat (str1:string) (str2:string) =
    System.String.Concat(str1, str2)

let (+) (str1:string) (str2:string) =
    // Remove characters from str 1 where they appear in str2, and return remainder in string 1
    str1.ToCharArray()
    |> Array.filter (fun c -> if str2.Contains(c) then true else false)
    |> (fun a -> new System.String(a))

let (-) (str1:string) (str2:string) =
    // Return characters from str 1 where they don't appear in str2
    str1.ToCharArray()
    |> Array.filter (fun c -> if str2.Contains(c) then false else true)
    |> (fun a -> new System.String(a))

let rec getDigitsByLen (entry:list<string>) (len:int) (results:list<string>) =
    match entry with
    | h::t -> if h.Length = len then getDigitsByLen t len results@[h] else getDigitsByLen t len results
    | _ -> results

let getCodes (data:string) =
    let codexAndCodes =
        data
        |> (fun i -> i.Split(" | "))
    let codex = codexAndCodes.[0].Split(" ") |> Array.map (fun s -> (new System.String(s.ToCharArray() |> Array.sort))) |> List.ofArray
    
    // Find 1, 4, 7, 8 - easy
    let one = (getDigitsByLen codex 2 List.empty<string>).Head
    let four = (getDigitsByLen codex 4 List.empty<string>).Head
    let seven = (getDigitsByLen codex 3 List.empty<string>).Head
    let eight = (getDigitsByLen codex 7 List.empty<string>).Head

    // Find all 3 patterns with 5 elements (2, 3, 5)
    let twoThreeFive = getDigitsByLen codex 5 List.empty<string>

    // Get the remaining 3 patterns with 6 elements (0, 6, 9)
    let zeroSixNine = getDigitsByLen codex 6 List.empty<string>

    // Use 4 to overlay test 2, 3 and 5 - the one that has only two elements matching is the number 2
    let two = twoThreeFive |> List.filter (fun s -> (s + four).Length = 2) |> (fun s -> s.Head)
    // Betwee 3 and 5, only 3 matches entirely with 1 so we can tell which is which
    let three = twoThreeFive |> List.filter (fun s -> not(s = two)) |> List.filter (fun s -> (s + one).Length = 2) |> (fun s -> s.Head)
    let five = twoThreeFive |> List.filter (fun s -> not(s = two) && not (s = three)) |> (fun s -> s.Head)

    // Now 0, 6 and 9: 9 is the only one that fully overlay matches with 4
    let nine = zeroSixNine |> List.filter (fun s -> (s + four).Length = 4) |> (fun s -> s.Head)
    // Between zero and six, only 0 fully matches with 1
    let zero = zeroSixNine |> List.filter (fun s -> not(s = nine)) |> List.filter (fun s -> (s + one).Length = 2) |> (fun s -> s.Head)
    let six = zeroSixNine |> List.filter (fun s -> not(s = nine) && not(s = zero)) |> (fun s -> s.Head)

    let codexSolver = [|zero;one;two;three;four;five;six;seven;eight;nine|]

    // // Which elements are which?
    // //  aaaa
    // // b    c
    // // b    c
    // //  dddd
    // // e    f
    // // e    f
    // //  gggg
    // // 
    // // We can find a by masking 1 and 7
    // let a = (seven - one)
    // // We can find d by masking 0 and 8
    // let d = (eight - zero)
    // // We can get c by masking 0 with 6 and d
    // let c = (zero - (six - d))
    // // We can get e by masking 0 with 9 and d
    // let e = (zero - (nine - d))
    // // The rest can be got from what we already know
    // let f = (one - c)
    // let g = (two - a - c - d - e)
    // let b = (eight - a - c - d - e - f - g)

    let codes = codexAndCodes.[1].Split(" ") |> Array.map (fun s -> (new System.String(s.ToCharArray() |> Array.sort))) |> List.ofArray

    codes
    |> List.map (fun c -> (codexSolver |> Array.findIndex (fun a -> a = c)))
    |> List.map string
    |> List.reduce concat
    |> int

let untangleSevenSegments (input:seq<string>) =
    input
    |> Seq.map getCodes
    |> Seq.sum
        
