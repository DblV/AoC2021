module Drive

let parseInstruction (i:string) =
    let separatorIndex = i.IndexOf(' ')
    (i.[0..separatorIndex-1] |> string, i.[separatorIndex+1..i.Length-1] |> int)

let applyInstruction (state:(int*int)) (instruction:(string*int)) =
    printfn "Instruction %s %i on state %i %i" (fst(instruction)) (snd(instruction)) (fst(state)) (snd(state))
    match fst(instruction) with
    | "up" -> (fst(state),snd(state)-snd(instruction))
    | "down" -> (fst(state),snd(state)+snd(instruction))
    | _ -> (fst(state)+snd(instruction),snd(state))

let followInstructions (instructions:seq<string>) =
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold applyInstruction (0,0)
    |> fun x -> fst(x) * snd(x)
