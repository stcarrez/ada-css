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
,(-7,7),(-6,5),(-5,4),(-4,3),(-3,2),(-2,1)
-- State  1
,(-7,7),(-6,5),(-5,4),(-3,11)
-- State  7
,(-4,13)
-- State  8
,(-4,15)
-- State  17
,(-4,20)
-- State  19
,(-4,21)
-- State  20
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,23),(-9,22),(-8,32)
-- State  21
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,23),(-9,22),(-8,33)
-- State  25
,(-4,37)
-- State  27
,(-16,44),(-4,39)
-- State  31
,(-4,46)
-- State  34
,(-4,47)
-- State  35
,(-4,48)
-- State  36
,(-4,49)
-- State  37
,(-17,28),(-15,27),(-14,50)
-- State  40
,(-4,52)
-- State  41
,(-16,53),(-4,39)
-- State  42
,(-16,54),(-4,39)
-- State  43
,(-16,55),(-4,39)
-- State  45
,(-4,56)
-- State  46
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,23),(-9,22),(-8,57)
-- State  47
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,58)
-- State  48
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,59)
-- State  49
,(-17,28),(-15,27),(-14,26),(-13,60)
-- State  56
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,23),(-10,63),(-9,64)
-- State  60
,(-4,37)
-- State  62
,(-4,68)
-- State  63
,(-4,69)
-- State  67
,(-4,73)
-- State  70
,(-4,75)
-- State  72
,(-4,76)
-- State  75
,(-17,28),(-15,27),(-14,26),(-13,25),(-12,24),(-11,23),(-9,77)
);
--  The offset vector
GOTO_OFFSET : array (0.. 77) of Integer :=
(0,
6,10,10,10,10,10,10,11,12,12,12,12,12,12,12,12,12,
13,13,14,22,30,30,30,30,31,31,33,33,33,33,34,34,34,
35,36,37,40,40,40,41,43,45,47,47,48,56,62,67,71,71,
71,71,71,71,71,79,79,79,79,80,80,81,82,82,82,82,83,
83,83,84,84,85,85,85,92, 92);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  40) of Natural := (2,
2,1,1,1,1,2,1,0,5,2,3,1,5,2,1,4,1,4,1,4,1,4,1,3,1,3,3,3,3,2,1,6,1,1,5,4,6,
5,4,1);
   Get_LHS_Rule: array (Rule range  0 ..  40) of Nonterminal := (-1,
-2,-2,-2,-3,-3,-4,-4,-4,-5,-5,-7,-7,-6,-6,
-8,-10,-10,-9,-9,-11,-11,-12,-12,-13,-13,-14,-14,-14,
-14,-14,-15,-15,-15,-15,-17,-17,-16,-16,-16,-16);
end Css.Analysis.Parser.Parser_Goto;
