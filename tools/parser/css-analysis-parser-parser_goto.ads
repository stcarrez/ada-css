pragma Style_Checks (Off);
package Css.Analysis.Parser.Parser_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-5,4),(-4,3),(-3,2),(-2,1)
-- State  1
,(-5,4),(-4,3),(-3,8)
-- State  5
,(-6,9)
-- State  6
,(-6,11)
-- State  13
,(-6,15)
-- State  14
,(-6,16)
-- State  15
,(-11,20),(-9,19),(-8,18),(-7,17)
-- State  16
,(-11,20),(-9,19),(-8,18),(-7,24)
-- State  17
,(-11,20),(-9,19),(-8,28)
-- State  19
,(-10,35),(-6,30)
-- State  23
,(-6,36)
-- State  24
,(-11,20),(-9,19),(-8,28)
-- State  25
,(-6,37)
-- State  26
,(-6,38)
-- State  27
,(-6,39)
-- State  31
,(-6,41)
-- State  32
,(-10,42),(-6,30)
-- State  33
,(-10,43),(-6,30)
-- State  34
,(-10,44),(-6,30)
-- State  36
,(-11,20),(-9,19),(-8,18),(-7,45)
-- State  37
,(-11,20),(-9,19),(-8,46)
-- State  38
,(-11,20),(-9,19),(-8,47)
-- State  39
,(-11,20),(-9,19),(-8,48)
-- State  45
,(-11,20),(-9,19),(-8,28)
-- State  50
,(-6,54)
-- State  53
,(-6,57)
-- State  56
,(-6,58)
);
--  The offset vector
GOTO_OFFSET : array (0.. 58) of Integer :=
(0,
4,7,7,7,7,8,9,9,9,9,9,9,9,10,11,15,19,
22,22,24,24,24,24,25,28,29,30,31,31,31,31,32,34,36,
38,38,42,45,48,51,51,51,51,51,51,54,54,54,54,54,55,
55,55,56,56,56,57, 57);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  28) of Natural := (2,
2,1,1,1,2,1,0,5,5,4,4,4,2,1,3,3,3,3,2,1,1,1,5,4,6,5,4,1);
   Get_LHS_Rule: array (Rule range  0 ..  28) of Nonterminal := (-1,
-2,-2,-3,-3,-6,-6,-6,-4,-5,-7,-7,-7,-7,-7,
-8,-8,-8,-8,-8,-9,-9,-9,-11,-11,-10,-10,-10,-10);
end Css.Analysis.Parser.Parser_Goto;
