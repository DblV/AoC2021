module Drive

let parseInstruction (i:string) =
    let separatorIndex = i.IndexOf(' ')
    (i.[0..separatorIndex-1] |> string, i.[separatorIndex+1..i.Length-1] |> int)

let applyInstructionWithoutAim (state:(int*int)) (instruction:(string*int)) =
    printfn "Instruction %s %i on state %i %i" (fst(instruction)) (snd(instruction)) (fst(state)) (snd(state))
    match fst(instruction) with
    | "up" -> (fst(state),snd(state)-snd(instruction))
    | "down" -> (fst(state),snd(state)+snd(instruction))
    | _ -> (fst(state)+snd(instruction),snd(state))

let applyInstruction (state:(int*int*int)) (instruction:(string*int)) =
    printfn "Instruction %s %i on state horiz:%i depth:%i aim:%i" (fst(instruction)) (snd(instruction)) (state|>fun (x,_,_) -> x) (state|>fun (_,x,_) -> x) (state|>fun (_,_,x) -> x)
    match fst(instruction) with
    | "up" -> (state|>fun(x,y,z) -> (x,y,z-snd(instruction)))
    | "down" -> (state|>fun(x,y,z) -> (x,y,z+snd(instruction)))
    | _ -> (state|>(fun(x,y,z) -> (x+snd(instruction),y+(snd(instruction)*z),z)))

let followInstructions (instructions:seq<string>) =
    instructions
    |> Seq.map parseInstruction
    |> Seq.fold applyInstruction (0,0,0)
    |> fun (x,y,_) -> x * y
