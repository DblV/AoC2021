let inputs = [ 1;2;3;4;5;6 ]

inputs 
|> List.pairwise 
|> List.pairwise
|> List.map (fun x -> fst(fst(x)) + fst(snd(x)) + snd(snd(x)))
