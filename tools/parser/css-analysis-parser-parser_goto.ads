pragma Style_Checks (Off);
package CSS.Analysis.Parser.Parser_Goto is

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
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-9,22),(-8,33)
-- State  21
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-9,22),(-8,34)
-- State  25
,(-4,38)
-- State  27
,(-18,46),(-4,41)
-- State  32
,(-4,48)
-- State  35
,(-4,49)
-- State  36
,(-4,50)
-- State  37
,(-4,51)
-- State  38
,(-17,27),(-16,52),(-12,28)
-- State  39
,(-4,54)
-- State  42
,(-4,56)
-- State  43
,(-18,57),(-4,41)
-- State  44
,(-18,58),(-4,41)
-- State  45
,(-18,59),(-4,41)
-- State  47
,(-4,60)
-- State  48
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-9,22),(-8,61)
-- State  49
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,62),(-12,28)
-- State  50
,(-17,27),(-16,26),(-15,25),(-14,63),(-12,28)
-- State  51
,(-17,27),(-16,26),(-15,64),(-12,28)
-- State  53
,(-4,65)
-- State  60
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,70),(-10,68),(-9,69)
-- State  64
,(-4,38)
-- State  65
,(-17,27),(-16,72),(-12,28)
-- State  67
,(-4,75)
-- State  68
,(-4,76)
-- State  74
,(-4,80)
-- State  76
,(-11,82)
-- State  77
,(-4,84)
-- State  79
,(-4,85)
-- State  81
,(-4,86)
-- State  84
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-11,89),(-9,87)
-- State  88
,(-4,91)
-- State  90
,(-4,92)
-- State  91
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-9,22),(-8,61)
-- State  92
,(-17,27),(-16,26),(-15,25),(-14,24),(-13,23),(-12,28),(-9,22),(-8,93)
);
--  The offset vector
GOTO_OFFSET : array (0.. 95) of Integer :=
(0,
6,10,10,10,10,10,10,11,12,12,12,12,12,12,12,12,12,
13,13,14,22,30,30,30,30,31,31,33,33,33,33,33,34,34,
34,35,36,37,40,41,41,41,42,44,46,48,48,49,57,63,68,
72,72,73,73,73,73,73,73,73,81,81,81,81,82,85,85,86,
87,87,87,87,87,87,88,88,89,90,90,91,91,92,92,92,100,
100,100,100,101,101,102,110,118,118, 118);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  48) of Natural := (2,
2,1,1,1,1,2,1,0,5,2,3,1,5,2,1,4,4,3,1,1,4,1,4,1,4,1,3,5,3,1,3,3,3,3,2,1,6,
1,1,1,7,6,5,4,6,5,4,1);
   Get_LHS_Rule: array (Rule range  0 ..  48) of Nonterminal := (-1,
-2,-2,-2,-3,-3,-4,-4,-4,-5,-5,-7,-7,-6,-6,
-8,-10,-10,-10,-10,-10,-9,-9,-13,-13,-14,-14,-15,-15,
-15,-15,-16,-16,-16,-16,-16,-17,-17,-17,-17,-17,-11,-11,
-12,-12,-18,-18,-18,-18);
end CSS.Analysis.Parser.Parser_Goto;
