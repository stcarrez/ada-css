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
,( 2, 9),( 3, 8),( 24, 6),(-1,-8)
-- State  1
,( 0,-3001),( 2, 9),( 3, 8),(-1,-3000)
-- State  2
,(-1,-2)
-- State  3
,( 24, 12),(-1,-3)
-- State  4
,(-1,-4)
-- State  5
,(-1,-5)
-- State  6
,(-1,-7)
-- State  7
,( 1, 14),( 24, 6),(-1,-8)
-- State  8
,( 1, 16),( 24, 6),(-1,-8)
-- State  9
,(-1,-12)
-- State  10
,(-1,-3000)
-- State  11
,(-1,-1)
-- State  12
,(-1,-6)
-- State  13
,( 2, 18),( 6, 17),( 24, 12),(-1,-3000)
-- State  14
,(-1,-10)
-- State  15
,( 16, 19),( 24, 12),(-1,-3000)
-- State  16
,(-1,-14)
-- State  17
,( 24, 6),(-1,-8)
-- State  18
,(-1,-11)
-- State  19
,( 24, 6),(-1,-8)
-- State  20
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  21
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  22
,( 7, 34),(-1,-15)
-- State  23
,( 8, 35),(-1,-19)
-- State  24
,( 17, 36),(-1,-21)
-- State  25
,( 4,-8),( 5,-8),( 14,-8),( 24, 6),(-1,-23)
-- State  26
,(-1,-25)
-- State  27
,( 12, 38),( 19, 41),( 20, 43),( 21, 40),( 22, 42),( 24, 6),(-1,-8)
-- State  28
,(-1,-31)
-- State  29
,( 10, 45),(-1,-33)
-- State  30
,(-1,-34)
-- State  31
,( 24, 6),(-1,-8)
-- State  32
,(-1,-9)
-- State  33
,(-1,-13)
-- State  34
,( 24, 6),(-1,-8)
-- State  35
,( 24, 6),(-1,-8)
-- State  36
,( 24, 6),(-1,-8)
-- State  37
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  38
,( 9, 51),(-1,-3000)
-- State  39
,( 24, 12),(-1,-40)
-- State  40
,( 24, 6),(-1,-8)
-- State  41
,( 12, 38),( 24, 6),(-1,-8)
-- State  42
,( 12, 38),( 24, 6),(-1,-8)
-- State  43
,( 12, 38),( 24, 6),(-1,-8)
-- State  44
,(-1,-30)
-- State  45
,( 24, 6),(-1,-8)
-- State  46
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  47
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  48
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  49
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  50
,(-1,-24)
-- State  51
,( 13, 62),( 23, 61),(-1,-3000)
-- State  52
,( 24, 12),(-1,-26)
-- State  53
,(-1,-27)
-- State  54
,(-1,-28)
-- State  55
,(-1,-29)
-- State  56
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  57
,( 15, 65),(-1,-3000)
-- State  58
,( 8, 35),(-1,-18)
-- State  59
,( 17, 36),(-1,-20)
-- State  60
,( 4,-8),( 5,-8),( 14,-8),( 24, 6),(-1,-22)
-- State  61
,( 9, 66),( 13, 67),(-1,-3000)
-- State  62
,( 24, 6),(-1,-8)
-- State  63
,( 23, 70),( 24, 6),(-1,-8)
-- State  64
,( 7, 34),(-1,-17)
-- State  65
,( 18, 71),(-1,-36)
-- State  66
,( 13, 72),(-1,-3000)
-- State  67
,( 24, 6),(-1,-8)
-- State  68
,( 24, 12),(-1,-39)
-- State  69
,( 11, 74),( 24, 12),(-1,-3000)
-- State  70
,( 24, 6),(-1,-8)
-- State  71
,(-1,-35)
-- State  72
,( 24, 6),(-1,-8)
-- State  73
,( 24, 12),(-1,-38)
-- State  74
,(-1,-32)
-- State  75
,( 4, 29),( 5, 30),( 14, 31),( 24, 12),(-1,-3000)
-- State  76
,( 24, 12),(-1,-37)
-- State  77
,( 7, 34),(-1,-16)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 77) of Integer :=
( 0,
 4, 8, 9, 11, 12, 13, 14, 17, 20, 21, 22, 23, 24, 28, 29, 32,
 33, 35, 36, 38, 43, 48, 50, 52, 54, 59, 60, 67, 68, 70, 71, 73,
 74, 75, 77, 79, 81, 86, 88, 90, 92, 95, 98, 101, 102, 104, 109, 114,
 119, 124, 125, 128, 130, 131, 132, 133, 138, 140, 142, 144, 149, 152, 154, 157,
 159, 161, 163, 165, 167, 170, 172, 173, 175, 177, 178, 183, 185);
end Css.Analysis.Parser.Parser_Shift_Reduce;
