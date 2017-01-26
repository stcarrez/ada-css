pragma Style_Checks (Off);
package CSS.Analysis.Parser.Parser_Shift_Reduce is

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
,( 1, 4),( 2, 8),( 3, 9),( 25, 6),(-1,-8)
-- State  1
,( 0,-3001),( 2, 8),( 3, 9),(-1,-3000)
-- State  2
,(-1,-2)
-- State  3
,( 25, 12),(-1,-3)
-- State  4
,(-1,-4)
-- State  5
,( 25, 6),(-1,-8)
-- State  6
,(-1,-7)
-- State  7
,( 1, 15),( 25, 6),(-1,-8)
-- State  8
,(-1,-14)
-- State  9
,(-1,-15)
-- State  10
,(-1,-3000)
-- State  11
,(-1,-1)
-- State  12
,(-1,-6)
-- State  13
,( 25, 12),(-1,-5)
-- State  14
,( 2, 18),( 3, 19),( 6, 16),( 16, 17),( 25, 12),(-1,-3000)
-- State  15
,(-1,-11)
-- State  16
,( 25, 6),(-1,-8)
-- State  17
,( 25, 6),(-1,-8)
-- State  18
,(-1,-12)
-- State  19
,(-1,-13)
-- State  20
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  21
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  22
,( 23,-8),( 25, 6),(-1,-16)
-- State  23
,( 7,-8),( 25, 6),(-1,-18)
-- State  24
,( 8,-8),( 25, 6),(-1,-20)
-- State  25
,( 17,-8),( 25, 6),(-1,-22)
-- State  26
,( 4,-8),( 5,-8),( 9,-8),( 14,-8),( 23,-8),( 24,-8),( 25, 6),(-1,-24)
-- State  27
,( 25, 6),(-1,-8)
-- State  28
,( 12, 45),( 19, 48),( 20, 50),( 21, 47),( 22, 49),( 25, 6),(-1,-8)
-- State  29
,(-1,-33)
-- State  30
,(-1,-34)
-- State  31
,( 25, 6),(-1,-8)
-- State  32
,( 10, 53),(-1,-36)
-- State  33
,(-1,-37)
-- State  34
,(-1,-38)
-- State  35
,(-1,-39)
-- State  36
,(-1,-40)
-- State  37
,(-1,-9)
-- State  38
,(-1,-10)
-- State  39
,( 23, 54),( 25, 12),(-1,-3000)
-- State  40
,( 7, 55),( 25, 12),(-1,-3000)
-- State  41
,( 8, 56),( 25, 12),(-1,-3000)
-- State  42
,( 17, 57),( 25, 12),(-1,-3000)
-- State  43
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 58),( 24, 36),( 25, 12),(-1,-3000)
-- State  44
,( 25, 12),(-1,-27)
-- State  45
,( 9, 60),(-1,-3000)
-- State  46
,( 25, 12),(-1,-51)
-- State  47
,(-1,-28)
-- State  48
,( 12, 45),( 25, 6),(-1,-8)
-- State  49
,( 12, 45),( 25, 6),(-1,-8)
-- State  50
,( 12, 45),( 25, 6),(-1,-8)
-- State  51
,(-1,-32)
-- State  52
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  53
,( 25, 6),(-1,-8)
-- State  54
,( 25, 6),(-1,-8)
-- State  55
,( 25, 6),(-1,-8)
-- State  56
,( 25, 6),(-1,-8)
-- State  57
,( 25, 6),(-1,-8)
-- State  58
,( 4,-8),( 5,-8),( 9,-8),( 14,-8),( 23,-8),( 24,-8),( 25, 6),(-1,-39)
-- State  59
,( 25, 6),(-1,-8)
-- State  60
,( 13, 73),( 23, 72),(-1,-3000)
-- State  61
,(-1,-29)
-- State  62
,(-1,-30)
-- State  63
,(-1,-31)
-- State  64
,( 15, 74),(-1,-3000)
-- State  65
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  66
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  67
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  68
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  69
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  70
,( 4, 32),( 5, 33),( 9, 34),( 14, 31),( 23, 35),( 24, 36),( 25, 12),(-1,-3000)
-- State  71
,( 25, 12),(-1,-25)
-- State  72
,( 9, 81),( 13, 82),(-1,-3000)
-- State  73
,(-1,-50)
-- State  74
,( 18, 83),(-1,-47)
-- State  75
,( 25, 6),(-1,-8)
-- State  76
,( 7,-8),( 25, 6),(-1,-17)
-- State  77
,( 8,-8),( 25, 6),(-1,-19)
-- State  78
,( 17,-8),( 25, 6),(-1,-21)
-- State  79
,( 4,-8),( 5,-8),( 9,-8),( 14,-8),( 23,-8),( 24,-8),( 25, 6),(-1,-23)
-- State  80
,( 25, 6),(-1,-8)
-- State  81
,( 13, 86),(-1,-3000)
-- State  82
,(-1,-49)
-- State  83
,(-1,-46)
-- State  84
,( 11, 87),( 25, 12),(-1,-3000)
-- State  85
,( 25, 12),(-1,-26)
-- State  86
,(-1,-48)
-- State  87
,(-1,-35)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 87) of Integer :=
( 0,
 5, 9, 10, 12, 13, 15, 16, 19, 20, 21, 22, 23, 24, 26, 32, 33,
 35, 37, 38, 39, 47, 55, 58, 61, 64, 67, 75, 77, 84, 85, 86, 88,
 90, 91, 92, 93, 94, 95, 96, 99, 102, 105, 108, 116, 118, 120, 122, 123,
 126, 129, 132, 133, 141, 143, 145, 147, 149, 151, 159, 161, 164, 165, 166, 167,
 169, 177, 185, 193, 201, 209, 217, 219, 222, 223, 225, 227, 230, 233, 236, 244,
 246, 248, 249, 250, 253, 255, 256);
end CSS.Analysis.Parser.Parser_Shift_Reduce;
