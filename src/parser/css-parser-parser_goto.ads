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
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,18),(-31,11),(-29,7),(-22,14),(-20,9)
,(-18,8),(-14,10),(-11,17),(-8,6),(-7,5),(-6,4),(-5,3),(-4,2),(-3,1),(-2,35)
-- State  1
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,18),(-31,11),(-29,7),(-22,14),(-20,9)
,(-18,8),(-14,10),(-11,17),(-8,6),(-7,5),(-6,4),(-5,3),(-4,36)
-- State  6
,(-14,37),(-11,17)
-- State  9
,(-11,41)
-- State  11
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,45),(-27,46)
-- State  13
,(-11,47)
-- State  14
,(-11,48)
-- State  18
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,51),(-11,50)
-- State  27
,(-39,29),(-38,28),(-37,57),(-36,56),(-35,55),(-34,52)
-- State  28
,(-39,60)
-- State  30
,(-11,61)
-- State  38
,(-11,69)
-- State  39
,(-11,70)
-- State  40
,(-11,71)
-- State  42
,(-11,73)
-- State  43
,(-11,74)
-- State  44
,(-11,75)
-- State  45
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,51),(-11,76)
-- State  46
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,77)
-- State  47
,(-19,79),(-17,78)
-- State  48
,(-24,82),(-23,81)
-- State  57
,(-11,85)
-- State  65
,(-11,89)
-- State  69
,(-44,92),(-30,90),(-25,91)
-- State  70
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,18),(-31,95)
-- State  71
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,18),(-31,11),(-29,7),(-5,96)
-- State  72
,(-11,97)
-- State  77
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,51),(-11,98)
-- State  80
,(-15,100)
-- State  81
,(-24,102)
-- State  84
,(-24,82),(-23,104)
-- State  86
,(-11,106)
-- State  89
,(-43,109),(-42,108),(-41,107)
-- State  91
,(-11,121)
-- State  93
,(-11,124)
-- State  95
,(-39,29),(-38,28),(-36,26),(-35,25),(-34,22),(-33,21),(-32,45),(-27,46)
-- State  97
,(-44,92),(-25,128),(-21,127)
-- State  99
,(-19,129)
-- State  104
,(-24,102)
-- State  106
,(-40,138)
-- State  110
,(-11,142)
-- State  111
,(-11,143)
-- State  112
,(-11,144)
-- State  113
,(-11,145)
-- State  114
,(-11,146)
-- State  115
,(-11,147)
-- State  116
,(-11,148)
-- State  117
,(-11,149)
-- State  118
,(-11,150)
-- State  119
,(-11,151)
-- State  120
,(-11,152)
-- State  122
,(-11,153)
-- State  125
,(-11,155)
-- State  126
,(-11,156)
-- State  128
,(-11,159)
-- State  138
,(-11,160)
-- State  140
,(-11,161)
-- State  141
,(-43,109),(-42,162)
-- State  151
,(-44,92),(-25,165)
-- State  153
,(-49,174),(-48,173),(-47,167),(-45,166),(-43,169),(-28,168)
-- State  157
,(-11,181)
-- State  158
,(-11,182)
-- State  163
,(-11,185)
-- State  164
,(-11,186)
-- State  165
,(-11,187)
-- State  166
,(-49,174),(-48,173),(-47,193),(-46,191),(-43,169),(-28,168),(-26,192)
-- State  168
,(-43,194)
-- State  170
,(-11,195)
-- State  171
,(-11,196)
-- State  172
,(-11,197)
-- State  178
,(-11,198)
-- State  179
,(-11,199)
-- State  181
,(-44,92),(-25,202)
-- State  183
,(-11,203)
-- State  184
,(-11,204)
-- State  185
,(-43,205)
-- State  188
,(-11,206)
-- State  189
,(-11,207)
-- State  190
,(-11,208)
-- State  192
,(-49,174),(-48,173),(-47,209),(-43,169),(-28,168)
-- State  199
,(-51,211),(-50,210),(-49,174),(-48,173),(-47,167),(-45,212),(-43,169),(-28,168)
-- State  201
,(-11,215)
-- State  202
,(-11,216)
-- State  210
,(-51,220),(-49,174),(-48,173),(-47,167),(-45,212),(-43,169),(-28,168)
-- State  212
,(-49,174),(-48,173),(-47,193),(-43,169),(-28,168),(-26,192)
-- State  213
,(-11,221)
-- State  214
,(-11,222)
-- State  219
,(-11,223)
-- State  224
,(-11,225)
-- State  225
,(-43,226)
-- State  226
,(-11,227)
);
--  The offset vector
GOTO_OFFSET : array (0.. 227) of Integer :=
(0,
21,40,40,40,40,40,42,42,42,43,43,51,51,52,53,53,53,
53,60,60,60,60,60,60,60,60,60,66,67,67,68,68,68,68,
68,68,68,68,69,70,71,71,72,73,74,81,88,90,92,92,92,
92,92,92,92,92,92,93,93,93,93,93,93,93,93,94,94,94,
94,97,105,115,116,116,116,116,116,123,123,123,124,125,125,125,127,
127,128,128,128,131,131,132,132,133,133,141,141,144,144,145,145,145,
145,145,146,146,147,147,147,147,148,149,150,151,152,153,154,155,156,
157,158,158,159,159,159,160,161,161,162,162,162,162,162,162,162,162,
162,162,163,163,164,166,166,166,166,166,166,166,166,166,166,168,168,
174,174,174,174,175,176,176,176,176,176,177,178,179,186,186,187,187,
188,189,190,190,190,190,190,190,191,192,192,194,194,195,196,197,197,
197,198,199,200,200,205,205,205,205,205,205,205,213,213,214,215,215,
215,215,215,215,215,215,222,222,228,229,230,230,230,230,230,231,231,
231,231,231,232,233, 234);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  134) of Natural := (2,
1,2,1,1,1,1,1,2,1,0,4,3,1,2,1,1,1,1,2,1,0,2,1,6,5,1,1,6,3,3,1,2,1,9,7,4,3,
1,2,1,0,2,5,2,2,2,2,2,2,1,1,8,6,6,4,1,1,4,3,2,2,1,1,1,1,1,1,4,2,2,1,2,1,1,
1,1,1,1,1,1,5,9,9,3,1,1,1,1,1,1,3,2,5,3,3,1,1,5,2,5,2,5,4,4,3,2,2,2,3,2,1,
2,1,2,2,2,1,1,1,2,2,2,2,2,2,2,2,5,4,2,1,1,6,2);
   Get_LHS_Rule: array (Rule range  0 ..  134) of Nonterminal := (-1,
-2,-3,-3,-4,-4,-4,-4,-9,-9,-9,-10,-12,-12,-8,
-8,-14,-14,-14,-11,-11,-11,-15,-15,-13,-13,-16,-16,-6,
-18,-17,-17,-19,-19,-7,-7,-20,-20,-22,-23,-23,-23,-24,
-21,-21,-26,-26,-27,-27,-27,-28,-28,-5,-5,-5,-29,-29,
-29,-31,-31,-31,-32,-32,-33,-33,-33,-33,-33,-33,-38,-38,
-38,-34,-34,-39,-39,-37,-37,-37,-37,-37,-35,-35,-35,-35,
-40,-40,-40,-40,-40,-40,-36,-36,-36,-41,-41,-41,-42,-42,
-42,-30,-30,-25,-25,-25,-25,-25,-44,-46,-45,-45,-45,-47,
-47,-47,-47,-47,-47,-47,-47,-43,-43,-43,-43,-43,-43,-43,
-43,-49,-49,-50,-50,-51,-51,-48);
end Css.Parser.Parser_Goto;
