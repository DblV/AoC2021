
type BingoCardNumber = {
    Value: int;
    Row: int;
    Col: int;
    Marked: bool
}

type BingoCard = {
    Numbers: list<BingoCardNumber>
}

let results =
  [{ Numbers =
      [{ Value = 7;
         Row = 4;
         Col = 4;
         Marked = true; }; { Value = 3;
                             Row = 3;
                             Col = 4;
                             Marked = false; }; { Value = 12;
                                                 Row = 2;
                                                 Col = 4;
                                                 Marked = false; };
       { Value = 0;
         Row = 1;
         Col = 4;
         Marked = false; }; { Value = 2;
                             Row = 0;
                             Col = 4;
                             Marked = false; }; { Value = 5;
                                                 Row = 4;
                                                 Col = 3;
                                                 Marked = true; };
       { Value = 6;
         Row = 3;
         Col = 3;
         Marked = false; }; { Value = 13;
                             Row = 2;
                             Col = 3;
                             Marked = true; }; { Value = 11;
                                                 Row = 1;
                                                 Col = 3;
                                                 Marked = false; };
       { Value = 22;
         Row = 0;
         Col = 3;
         Marked = false; }; { Value = 20;
                             Row = 4;
                             Col = 2;
                             Marked = true; }; { Value = 26;
                                                 Row = 3;
                                                 Col = 2;
                                                 Marked = false; };
       { Value = 23;
         Row = 2;
         Col = 2;
         Marked = true; }; { Value = 8;
                             Row = 1;
                             Col = 2;
                             Marked = false; }; { Value = 18;
                                                 Row = 0;
                                                 Col = 2;
                                                 Marked = false; };
       { Value = 19;
         Row = 4;
         Col = 1;
         Marked = true; }; { Value = 9;
                             Row = 3;
                             Col = 1;
                             Marked = false; }; { Value = 15;
                                                 Row = 2;
                                                 Col = 1;
                                                 Marked = false; };
       { Value = 16;
         Row = 1;
         Col = 1;
         Marked = false; }; { Value = 10;
                             Row = 0;
                             Col = 1;
                             Marked = false; }; { Value = 4;
                                                 Row = 4;
                                                 Col = 0;
                                                 Marked = true; };
       { Value = 24;
         Row = 3;
         Col = 0;
         Marked = false; }; { Value = 17;
                             Row = 2;
                             Col = 0;
                             Marked = false; }; { Value = 21;
                                                 Row = 1;
                                                 Col = 0;
                                                 Marked = false; };
       { Value = 14;
         Row = 0;
         Col = 0;
         Marked = false; }] };]