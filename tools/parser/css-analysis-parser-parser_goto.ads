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
,(-6,7),(-5,5),(-4,3),(-3,2),(-2,1)
-- State  1
,(-6,7),(-5,5),(-3,11)
-- State  5
,(-4,13)
-- State  7
,(-4,14)
-- State  16
,(-4,20)
-- State  17
,(-4,21)
-- State  20
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,24),(-9,23),(-8,22),(-7,37)
-- State  21
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,24),(-9,23),(-8,22),(-7,38)
-- State  22
,(-4,39)
-- State  23
,(-4,40)
-- State  24
,(-4,41)
-- State  25
,(-4,42)
-- State  26
,(-4,43)
-- State  27
,(-4,44)
-- State  28
,(-15,51),(-4,46)
-- State  31
,(-4,52)
-- State  43
,(-17,30),(-16,29),(-14,28),(-13,59)
-- State  48
,(-15,61),(-4,46)
-- State  49
,(-15,62),(-4,46)
-- State  50
,(-15,63),(-4,46)
-- State  52
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,24),(-9,23),(-8,22),(-7,64)
-- State  53
,(-4,65)
-- State  54
,(-4,66)
-- State  55
,(-4,67)
-- State  56
,(-4,68)
-- State  57
,(-4,69)
-- State  58
,(-4,70)
-- State  59
,(-4,71)
-- State  65
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,24),(-9,23),(-8,22),(-7,75)
-- State  66
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,24),(-9,76)
-- State  67
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,25),(-10,77)
-- State  68
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,26),(-11,78)
-- State  69
,(-17,30),(-16,29),(-14,28),(-13,27),(-12,79)
-- State  70
,(-17,30),(-16,29),(-14,28),(-13,80)
-- State  75
,(-4,84)
-- State  76
,(-4,40)
-- State  77
,(-4,41)
-- State  78
,(-4,42)
-- State  79
,(-4,43)
-- State  80
,(-4,85)
);
--  The offset vector
GOTO_OFFSET : array (0.. 87) of Integer :=
(0,
5,8,8,8,8,9,9,10,10,10,10,10,10,10,10,10,11,
12,12,12,22,32,33,34,35,36,37,38,40,40,40,41,41,41,
41,41,41,41,41,41,41,41,41,45,45,45,45,45,47,49,51,
51,61,62,63,64,65,66,67,68,68,68,68,68,68,78,86,93,
99,104,108,108,108,108,108,109,110,111,112,113,114,114,114,114,114,
114, 114);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  51) of Natural := (2,
2,1,1,1,2,2,1,0,5,5,2,3,3,1,1,1,5,1,5,1,5,1,5,1,4,6,2,2,3,3,3,2,1,1,6,1,1,
1,1,1,3,1,5,4,1,5,4,5,4,3,1);
   Get_LHS_Rule: array (Rule range  0 ..  51) of Nonterminal := (-1,
-2,-2,-2,-2,-3,-4,-4,-4,-5,-5,-5,-6,-6,-6,
-6,-7,-8,-8,-9,-9,-10,-10,-11,-11,-12,-12,-12,-13,
-13,-13,-13,-13,-14,-14,-17,-17,-17,-17,-17,-17,-18,-18,
-19,-19,-19,-16,-16,-15,-15,-15,-15);
end CSS.Analysis.Parser.Parser_Goto;
