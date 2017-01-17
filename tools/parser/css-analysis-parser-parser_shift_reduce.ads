pragma Style_Checks (Off);
package Css.Analysis.Parser.Parser_Shift_Reduce is

    type Small_Integer is range -32_000 .. 32_000;

    type Shift_Reduce_Entry is record
        T   : Small_Integer;
        Act : Small_Integer;
    end record;
    pragma Pack(Shift_Reduce_Entry);

    subtype Row is Integer range -1 .. Integer'Last;

  --pragma suppress(index_check);

    type Shift_Reduce_Array is array (Row  range <>) of Shift_Reduce_Entry;

    Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
        ( (-1,-1) -- Dummy Entry

-- State  0
,( 2, 5),( 3, 6),(-1,-3000)
-- State  1
,( 0,-3001),( 2, 5),( 3, 6),(-1,-3000)
-- State  2
,(-1,-2)
-- State  3
,(-1,-3)
-- State  4
,(-1,-4)
-- State  5
,( 24, 10),(-1,-7)
-- State  6
,( 24, 10),(-1,-7)
-- State  7
,(-1,-3000)
-- State  8
,(-1,-1)
-- State  9
,( 6, 13),( 24, 12),(-1,-3000)
-- State  10
,(-1,-6)
-- State  11
,( 16, 14),( 24, 12),(-1,-3000)
-- State  12
,(-1,-5)
-- State  13
,( 24, 10),(-1,-7)
-- State  14
,( 24, 10),(-1,-7)
-- State  15
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  16
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  17
,( 4, 22),( 5, 21),( 7, 25),( 8, 26),( 14, 23),( 17, 27),(-1,-8)
-- State  18
,(-1,-14)
-- State  19
,( 12, 29),( 19, 32),( 20, 34),( 21, 31),( 22, 33),( 24, 10),(-1,-7)
-- State  20
,(-1,-20)
-- State  21
,(-1,-21)
-- State  22
,(-1,-22)
-- State  23
,( 24, 10),(-1,-7)
-- State  24
,( 4, 22),( 5, 21),( 7, 25),( 8, 26),( 14, 23),( 17, 27),(-1,-9)
-- State  25
,( 24, 10),(-1,-7)
-- State  26
,( 24, 10),(-1,-7)
-- State  27
,( 24, 10),(-1,-7)
-- State  28
,(-1,-13)
-- State  29
,( 9, 40),(-1,-3000)
-- State  30
,( 24, 12),(-1,-28)
-- State  31
,( 24, 10),(-1,-7)
-- State  32
,( 12, 29),( 24, 10),(-1,-7)
-- State  33
,( 12, 29),( 24, 10),(-1,-7)
-- State  34
,( 12, 29),( 24, 10),(-1,-7)
-- State  35
,(-1,-19)
-- State  36
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  37
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  38
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  39
,( 4, 22),( 5, 21),( 14, 23),( 24, 12),(-1,-3000)
-- State  40
,( 13, 50),( 23, 49),(-1,-3000)
-- State  41
,( 24, 12),(-1,-15)
-- State  42
,(-1,-16)
-- State  43
,(-1,-17)
-- State  44
,(-1,-18)
-- State  45
,( 4, 22),( 5, 21),( 7, 25),( 8, 26),( 14, 23),( 15, 51),( 17, 27),(-1,-3000)
-- State  46
,(-1,-10)
-- State  47
,(-1,-11)
-- State  48
,(-1,-12)
-- State  49
,( 9, 52),( 13, 53),(-1,-3000)
-- State  50
,( 24, 10),(-1,-7)
-- State  51
,( 18, 55),(-1,-24)
-- State  52
,( 13, 56),(-1,-3000)
-- State  53
,( 24, 10),(-1,-7)
-- State  54
,( 24, 12),(-1,-27)
-- State  55
,(-1,-23)
-- State  56
,( 24, 10),(-1,-7)
-- State  57
,( 24, 12),(-1,-26)
-- State  58
,( 24, 12),(-1,-25)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 58) of Integer :=
( 0,
 3, 7, 8, 9, 10, 12, 14, 15, 16, 19, 20, 23, 24, 26, 28, 33,
 38, 45, 46, 53, 54, 55, 56, 58, 65, 67, 69, 71, 72, 74, 76, 78,
 81, 84, 87, 88, 93, 98, 103, 108, 111, 113, 114, 115, 116, 124, 125, 126,
 127, 130, 132, 134, 136, 138, 140, 141, 143, 145);
end Css.Analysis.Parser.Parser_Shift_Reduce;
