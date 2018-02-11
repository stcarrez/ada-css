pragma Style_Checks (Off);
package CSS.Parser.Parser_Goto is

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
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-44,20)
,(-42,13),(-28,15),(-22,14),(-20,12),(-16,17),(-13,25),(-11,10),(-10,9),(-9,8),(-8,7),(-7,6)
,(-6,5),(-5,4),(-4,2),(-3,1),(-2,44)
-- State  1
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-44,20)
,(-42,13),(-28,15),(-22,14),(-20,12),(-16,17),(-13,25),(-11,10),(-10,9),(-9,8),(-8,7),(-7,6)
,(-6,5),(-5,4),(-4,46)
-- State  10
,(-16,49),(-13,25)
-- State  11
,(-13,50)
-- State  13
,(-13,52)
-- State  14
,(-13,53)
-- State  19
,(-13,58)
-- State  20
,(-13,59)
-- State  21
,(-13,60)
-- State  22
,(-13,62)
-- State  26
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,67),(-49,68)
-- State  29
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,70),(-13,69)
-- State  36
,(-63,38),(-62,37),(-61,76),(-60,75),(-59,74),(-58,71)
-- State  37
,(-63,79)
-- State  39
,(-13,80)
-- State  47
,(-13,88)
-- State  48
,(-13,89)
-- State  50
,(-67,92),(-52,90),(-47,91),(-27,96)
-- State  51
,(-13,98)
-- State  53
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-26,103)
,(-25,101),(-23,100),(-5,102)
-- State  54
,(-13,113)
-- State  57
,(-13,115)
-- State  58
,(-38,128),(-37,127),(-36,125),(-35,124),(-34,123),(-31,119),(-30,118),(-29,117),(-19,116)
-- State  59
,(-46,132),(-45,131)
-- State  60
,(-73,144),(-72,137),(-71,136),(-70,145),(-26,141),(-24,135)
-- State  64
,(-13,150)
-- State  65
,(-13,151)
-- State  66
,(-13,152)
-- State  67
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,70),(-13,153)
-- State  68
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,154)
-- State  76
,(-13,155)
-- State  84
,(-13,159)
-- State  88
,(-67,92),(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-52,90)
,(-51,11),(-47,91),(-27,165),(-21,160),(-5,161)
-- State  90
,(-53,166)
-- State  91
,(-13,169)
-- State  94
,(-13,173)
-- State  97
,(-13,176)
-- State  98
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-21,177)
,(-5,161)
-- State  99
,(-13,178)
-- State  100
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-26,103)
,(-25,180),(-5,102)
-- State  105
,(-13,183)
-- State  106
,(-13,184)
-- State  107
,(-13,185)
-- State  108
,(-13,186)
-- State  109
,(-13,187)
-- State  110
,(-13,188)
-- State  111
,(-13,189)
-- State  112
,(-13,190)
-- State  113
,(-67,92),(-52,90),(-47,91),(-27,192)
-- State  114
,(-13,193)
-- State  115
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,194)
-- State  120
,(-13,196)
-- State  121
,(-13,197)
-- State  122
,(-13,198)
-- State  124
,(-37,199)
-- State  125
,(-38,200)
-- State  126
,(-13,201)
-- State  129
,(-13,203)
-- State  130
,(-34,204)
-- State  131
,(-46,205)
-- State  134
,(-46,132),(-45,207)
-- State  135
,(-73,144),(-72,137),(-71,208),(-70,145),(-26,141)
-- State  136
,(-73,144),(-72,214),(-70,145),(-26,141)
-- State  138
,(-13,215)
-- State  139
,(-26,216)
-- State  140
,(-26,217)
-- State  142
,(-13,218)
-- State  143
,(-13,219)
-- State  147
,(-13,220)
-- State  148
,(-13,221)
-- State  154
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,70),(-13,223)
-- State  156
,(-13,225)
-- State  159
,(-66,227),(-65,226),(-26,228)
-- State  160
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-5,231)
-- State  163
,(-13,173)
-- State  166
,(-67,92),(-47,233)
-- State  167
,(-13,235)
-- State  168
,(-53,236)
-- State  170
,(-13,237)
-- State  172
,(-13,239)
-- State  174
,(-13,240)
-- State  175
,(-13,241)
-- State  177
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,29),(-55,26),(-54,18),(-51,11),(-5,231)
-- State  178
,(-67,92),(-47,244),(-43,243)
-- State  179
,(-13,245)
-- State  181
,(-13,246)
-- State  194
,(-63,38),(-62,37),(-60,35),(-59,34),(-58,31),(-57,30),(-56,67),(-49,68)
-- State  195
,(-13,248)
-- State  196
,(-34,250)
-- State  198
,(-32,253)
-- State  201
,(-39,258),(-38,128),(-37,127),(-36,125),(-35,124),(-34,123),(-31,257),(-26,256)
-- State  203
,(-34,260)
-- State  207
,(-46,205)
-- State  208
,(-73,144),(-72,214),(-70,145),(-26,141)
-- State  210
,(-13,261)
-- State  211
,(-26,216),(-13,262)
-- State  212
,(-26,217),(-13,263)
-- State  213
,(-13,264)
-- State  221
,(-73,144),(-72,137),(-71,136),(-70,145),(-26,141),(-24,266)
-- State  225
,(-64,276)
-- State  229
,(-13,280)
-- State  230
,(-13,281)
-- State  232
,(-13,282)
-- State  233
,(-13,283)
-- State  234
,(-13,284)
-- State  236
,(-67,92),(-47,285)
-- State  237
,(-73,144),(-72,287),(-70,145),(-68,286),(-26,141)
-- State  242
,(-13,289)
-- State  244
,(-13,292)
-- State  246
,(-67,92),(-52,90),(-47,91),(-27,293)
-- State  247
,(-13,294)
-- State  248
,(-38,128),(-37,127),(-36,125),(-35,124),(-34,123),(-31,119),(-30,118),(-29,295)
-- State  249
,(-13,296)
-- State  251
,(-13,297)
-- State  252
,(-13,298)
-- State  254
,(-13,299)
-- State  255
,(-13,300)
-- State  256
,(-13,301)
-- State  257
,(-13,302)
-- State  258
,(-13,303)
-- State  259
,(-13,304)
-- State  261
,(-73,144),(-72,137),(-71,305),(-70,145),(-26,141)
-- State  262
,(-73,144),(-72,306),(-70,145),(-26,141)
-- State  263
,(-73,144),(-72,307),(-70,145),(-26,141)
-- State  265
,(-13,308)
-- State  266
,(-73,144),(-72,137),(-71,208),(-70,145),(-26,141)
-- State  267
,(-13,310)
-- State  268
,(-13,311)
-- State  276
,(-13,312)
-- State  278
,(-13,313)
-- State  279
,(-66,314),(-26,228)
-- State  285
,(-13,316)
-- State  286
,(-73,144),(-72,322),(-70,145),(-69,320),(-48,321),(-26,141)
-- State  290
,(-13,323)
-- State  291
,(-13,324)
-- State  296
,(-32,326)
-- State  297
,(-32,327)
-- State  298
,(-34,329),(-33,330)
-- State  299
,(-34,250)
-- State  300
,(-40,334)
-- State  301
,(-40,336)
-- State  305
,(-73,144),(-72,214),(-70,145),(-26,141)
-- State  308
,(-26,339)
-- State  309
,(-13,340)
-- State  315
,(-13,343)
-- State  317
,(-13,344)
-- State  318
,(-13,345)
-- State  319
,(-13,346)
-- State  321
,(-73,144),(-72,347),(-70,145),(-26,141)
-- State  323
,(-67,92),(-47,349)
-- State  325
,(-13,350)
-- State  328
,(-13,351)
-- State  329
,(-37,127),(-35,352)
-- State  333
,(-13,355)
-- State  334
,(-13,356)
-- State  335
,(-13,357)
-- State  336
,(-13,358)
-- State  337
,(-13,359)
-- State  338
,(-13,360)
-- State  339
,(-13,361)
-- State  341
,(-13,362)
-- State  342
,(-13,363)
-- State  343
,(-26,364)
-- State  348
,(-13,365)
-- State  349
,(-13,366)
-- State  351
,(-34,367)
-- State  352
,(-37,199)
-- State  356
,(-26,368)
-- State  357
,(-41,371),(-26,369)
-- State  372
,(-13,375)
-- State  375
,(-26,376)
);
--  The offset vector
GOTO_OFFSET : array (0.. 376) of Integer :=
(0,
27,52,52,52,52,52,52,52,52,52,54,55,55,56,57,57,57,
57,57,58,59,60,61,61,61,61,69,69,69,76,76,76,76,76,
76,76,82,83,83,84,84,84,84,84,84,84,84,85,86,86,90,
91,91,105,106,106,106,107,116,118,124,124,124,124,125,126,127,134,
141,141,141,141,141,141,141,141,142,142,142,142,142,142,142,142,143,
143,143,143,159,159,160,161,161,161,162,162,162,163,175,176,189,189,
189,189,189,190,191,192,193,194,195,196,197,201,202,210,210,210,210,
210,211,212,213,213,214,215,216,216,216,217,218,219,219,219,221,226,
230,230,231,232,233,233,234,235,235,235,235,236,237,237,237,237,237,
237,244,244,245,245,245,248,259,259,259,260,260,260,262,263,264,264,
265,265,266,266,267,268,268,279,282,283,283,284,284,284,284,284,284,
284,284,284,284,284,284,284,292,293,294,294,295,295,295,303,303,304,
304,304,304,305,309,309,310,312,314,315,315,315,315,315,315,315,315,
321,321,321,321,322,322,322,322,323,324,324,325,326,327,327,329,334,
334,334,334,334,335,335,336,336,340,341,349,350,350,351,352,352,353,
354,355,356,357,358,358,363,367,371,371,372,377,378,379,379,379,379,
379,379,379,379,380,380,381,383,383,383,383,383,383,384,390,390,390,
390,391,392,392,392,392,392,393,394,396,397,398,399,399,399,399,403,
403,403,404,405,405,405,405,405,405,406,406,407,408,409,409,413,413,
415,415,416,416,416,417,419,419,419,419,420,421,422,423,424,425,426,
426,427,428,429,429,429,429,429,430,431,431,432,433,433,433,433,434,
436,436,436,436,436,436,436,436,436,436,436,436,436,436,436,437,437,
437, 438);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  202) of Natural := (2,
1,2,1,1,1,1,1,1,1,1,1,2,1,0,4,3,1,2,1,1,1,1,2,1,0,2,1,6,5,1,1,6,6,2,1,0,3,
1,5,4,3,2,1,1,6,6,6,2,4,1,1,1,5,5,3,3,0,3,1,1,1,3,2,1,2,1,3,2,1,2,6,6,6,4,
2,2,1,1,5,7,5,5,1,1,1,9,7,4,3,1,2,1,0,2,5,2,2,2,2,2,2,1,1,5,5,4,3,2,1,2,2,
4,1,4,3,2,2,1,1,1,1,1,1,4,2,2,1,2,1,1,1,1,1,1,1,1,5,9,9,3,1,1,1,1,1,1,3,2,
5,3,3,1,1,5,2,4,5,3,2,3,2,5,4,4,3,2,1,2,3,2,5,4,4,4,2,1,4,4,2,1,6,3,2,1,2,
2,1,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2);
   Get_LHS_Rule: array (Rule range  0 ..  202) of Nonterminal := (-1,
-2,-3,-3,-3,-4,-4,-4,-4,-4,-4,-4,-12,-12,-12,
-10,-14,-14,-11,-11,-16,-16,-16,-13,-13,-13,-17,-17,-15,
-15,-18,-18,-6,-6,-21,-21,-21,-20,-20,-8,-22,-22,-23,
-23,-25,-25,-9,-9,-28,-19,-19,-29,-30,-30,-30,-30,-32,
-32,-31,-31,-31,-31,-33,-33,-33,-35,-35,-37,-36,-36,-38,
-34,-34,-34,-34,-40,-40,-40,-40,-39,-39,-39,-39,-39,-41,
-41,-7,-7,-42,-42,-44,-45,-45,-45,-46,-43,-43,-48,-48,
-49,-49,-49,-50,-50,-5,-5,-5,-5,-27,-27,-51,-51,-54,
-54,-55,-55,-55,-56,-56,-57,-57,-57,-57,-57,-57,-62,-62,
-62,-58,-58,-63,-63,-61,-61,-61,-61,-61,-59,-59,-59,-59,
-64,-64,-64,-64,-64,-64,-60,-60,-60,-65,-65,-65,-66,-66,
-66,-52,-52,-52,-52,-53,-53,-47,-47,-47,-47,-47,-47,-67,
-67,-69,-70,-70,-70,-24,-24,-24,-71,-71,-71,-71,-71,-68,
-68,-68,-72,-72,-72,-72,-72,-72,-72,-72,-72,-26,-26,-26,
-26,-26,-26,-26,-26,-73);
end CSS.Parser.Parser_Goto;
