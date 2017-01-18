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
,(-6,5),(-5,4),(-4,3),(-3,2),(-2,1)
-- State  1
,(-6,5),(-5,4),(-3,10)
-- State  7
,(-4,12)
-- State  8
,(-4,14)
-- State  16
,(-4,18)
-- State  17
,(-4,19)
-- State  18
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,21),(-8,20),(-7,30)
-- State  19
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,21),(-8,20),(-7,31)
-- State  23
,(-4,35)
-- State  25
,(-15,42),(-4,37)
-- State  29
,(-4,44)
-- State  32
,(-4,45)
-- State  33
,(-4,46)
-- State  34
,(-4,47)
-- State  35
,(-16,26),(-14,25),(-13,48)
-- State  38
,(-4,50)
-- State  39
,(-15,51),(-4,37)
-- State  40
,(-15,52),(-4,37)
-- State  41
,(-15,53),(-4,37)
-- State  43
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,21),(-9,54),(-8,55)
-- State  44
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,21),(-8,20),(-7,56)
-- State  45
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,57)
-- State  46
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,58)
-- State  47
,(-16,26),(-14,25),(-13,24),(-12,59)
-- State  59
,(-4,35)
-- State  61
,(-4,67)
-- State  62
,(-4,68)
-- State  66
,(-4,71)
-- State  68
,(-16,26),(-14,25),(-13,24),(-12,23),(-11,22),(-10,21),(-8,72)
-- State  70
,(-4,73)
);
--  The offset vector
GOTO_OFFSET : array (0.. 73) of Integer :=
(0,
5,8,8,8,8,8,8,9,10,10,10,10,10,10,10,10,11,
12,20,28,28,28,28,29,29,31,31,31,31,32,32,32,33,34,
35,38,38,38,39,41,43,45,45,53,61,67,72,76,76,76,76,
76,76,76,76,76,76,76,76,77,77,78,79,79,79,79,80,80,
87,87,88,88, 88);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  38) of Natural := (2,
2,1,1,1,1,2,1,0,5,2,5,2,1,4,1,4,1,4,1,4,1,3,1,3,3,3,3,2,1,4,1,1,5,4,6,5,4,
1);
   Get_LHS_Rule: array (Rule range  0 ..  38) of Nonterminal := (-1,
-2,-2,-2,-3,-3,-4,-4,-4,-5,-5,-6,-6,-7,-9,
-9,-8,-8,-10,-10,-11,-11,-12,-12,-13,-13,-13,-13,-13,
-14,-14,-14,-14,-16,-16,-15,-15,-15,-15);
end Css.Analysis.Parser.Parser_Goto;
