pragma Style_Checks (Off);
package Css.Parser.Parser_Goto is

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
,(-6,4),(-4,3),(-3,2),(-2,1)
-- State  1
,(-6,4),(-4,3),(-3,9)
-- State  3
,(-5,11)
-- State  4
,(-6,13)
-- State  10
,(-9,22),(-8,20),(-7,19)
-- State  12
,(-6,24)
-- State  13
,(-6,13)
-- State  14
,(-6,25)
-- State  15
,(-6,26)
-- State  16
,(-6,27)
-- State  19
,(-9,22),(-8,29)
-- State  24
,(-6,13)
-- State  25
,(-6,13)
-- State  26
,(-6,13)
-- State  27
,(-6,13)
-- State  33
,(-15,40),(-14,39),(-13,38),(-12,36),(-11,51),(-10,35)
-- State  35
,(-15,40),(-14,39),(-13,38),(-12,53),(-11,52)
-- State  48
,(-18,58),(-17,57),(-16,56),(-13,59)
-- State  49
,(-19,62),(-15,40),(-14,39),(-13,38),(-12,63)
-- State  68
,(-18,58),(-17,75),(-13,59)
-- State  69
,(-18,58),(-17,76),(-13,59)
-- State  70
,(-18,77),(-13,59)
-- State  71
,(-18,78),(-13,59)
-- State  73
,(-15,40),(-14,39),(-13,38),(-12,79)
-- State  80
,(-15,40),(-14,39),(-13,38),(-12,36),(-10,82)
-- State  82
,(-15,40),(-14,39),(-13,38),(-12,53)
);
--  The offset vector
GOTO_OFFSET : array (0.. 83) of Integer :=
(0,
4,7,7,8,9,9,9,9,9,9,12,12,13,14,15,16,17,
17,17,19,19,19,19,19,20,21,22,23,23,23,23,23,23,29,
29,34,34,34,34,34,34,34,34,34,34,34,34,34,38,43,43,
43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,
46,49,51,53,53,57,57,57,57,57,57,57,62,62, 66);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  53) of Natural := (2,
2,1,2,3,1,0,3,3,3,2,2,2,1,1,1,3,3,3,2,2,4,3,3,2,1,1,1,1,1,1,1,1,1,1,3,3,3,
3,3,1,3,3,1,1,1,1,3,3,1,6,4,1,2);
   Get_LHS_Rule: array (Rule range  0 ..  53) of Nonterminal := (-1,
-2,-2,-3,-4,-4,-4,-6,-6,-6,-6,-6,-6,-6,-6,
-6,-5,-5,-7,-7,-7,-8,-8,-8,-10,-10,-12,-12,-12,
-12,-12,-12,-12,-12,-12,-12,-13,-13,-16,-16,-16,-17,-17,
-17,-18,-18,-18,-14,-19,-19,-15,-15,-9,-11);
end Css.Parser.Parser_Goto;
