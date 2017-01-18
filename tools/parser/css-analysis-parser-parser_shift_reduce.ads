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
,( 2, 7),( 3, 8),( 24, 6),(-1,-8)
-- State  1
,( 0,-3001),( 2, 7),( 3, 8),(-1,-3000)
-- State  2
,(-1,-2)
-- State  3
,( 24, 11),(-1,-3)
-- State  4
,(-1,-4)
-- State  5
,(-1,-5)
-- State  6
,(-1,-7)
-- State  7
,( 1, 13),( 24, 6),(-1,-8)
-- State  8
,( 1, 15),( 24, 6),(-1,-8)
-- State  9
,(-1,-3000)
-- State  10
,(-1,-1)
-- State  11
,(-1,-6)
-- State  12
,( 6, 16),( 24, 11),(-1,-3000)
-- State  13
,(-1,-10)
-- State  14
,( 16, 17),( 24, 11),(-1,-3000)
-- State  15
,(-1,-12)
-- State  16
,( 24, 6),(-1,-8)
-- State  17
,( 24, 6),(-1,-8)
-- State  18
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  19
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  20
,( 7, 32),(-1,-13)
-- State  21
,( 8, 33),(-1,-17)
-- State  22
,( 17, 34),(-1,-19)
-- State  23
,( 4,-8),( 5,-8),( 14,-8),( 24, 6),(-1,-21)
-- State  24
,(-1,-23)
-- State  25
,( 12, 36),( 19, 39),( 20, 41),( 21, 38),( 22, 40),( 24, 6),(-1,-8)
-- State  26
,(-1,-29)
-- State  27
,( 10, 43),(-1,-31)
-- State  28
,(-1,-32)
-- State  29
,( 24, 6),(-1,-8)
-- State  30
,(-1,-9)
-- State  31
,(-1,-11)
-- State  32
,( 24, 6),(-1,-8)
-- State  33
,( 24, 6),(-1,-8)
-- State  34
,( 24, 6),(-1,-8)
-- State  35
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  36
,( 9, 49),(-1,-3000)
-- State  37
,( 24, 11),(-1,-38)
-- State  38
,( 24, 6),(-1,-8)
-- State  39
,( 12, 36),( 24, 6),(-1,-8)
-- State  40
,( 12, 36),( 24, 6),(-1,-8)
-- State  41
,( 12, 36),( 24, 6),(-1,-8)
-- State  42
,(-1,-28)
-- State  43
,( 4, 27),( 5, 28),( 14, 29),(-1,-3000)
-- State  44
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  45
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  46
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  47
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  48
,(-1,-22)
-- State  49
,( 13, 61),( 23, 60),(-1,-3000)
-- State  50
,( 24, 11),(-1,-24)
-- State  51
,(-1,-25)
-- State  52
,(-1,-26)
-- State  53
,(-1,-27)
-- State  54
,( 23, 63),( 24, 6),(-1,-8)
-- State  55
,( 7, 32),(-1,-15)
-- State  56
,( 15, 64),(-1,-3000)
-- State  57
,( 8, 33),(-1,-16)
-- State  58
,( 17, 34),(-1,-18)
-- State  59
,( 4,-8),( 5,-8),( 14,-8),( 24, 6),(-1,-20)
-- State  60
,( 9, 65),( 13, 66),(-1,-3000)
-- State  61
,( 24, 6),(-1,-8)
-- State  62
,( 11, 68),( 24, 11),(-1,-3000)
-- State  63
,( 24, 6),(-1,-8)
-- State  64
,( 18, 70),(-1,-34)
-- State  65
,( 13, 71),(-1,-3000)
-- State  66
,( 24, 6),(-1,-8)
-- State  67
,( 24, 11),(-1,-37)
-- State  68
,(-1,-30)
-- State  69
,( 4, 27),( 5, 28),( 14, 29),( 24, 11),(-1,-3000)
-- State  70
,(-1,-33)
-- State  71
,( 24, 6),(-1,-8)
-- State  72
,( 24, 11),(-1,-36)
-- State  73
,( 7, 32),(-1,-14)
-- State  74
,( 24, 11),(-1,-35)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 74) of Integer :=
( 0,
 4, 8, 9, 11, 12, 13, 14, 17, 20, 21, 22, 23, 26, 27, 30, 31,
 33, 35, 40, 45, 47, 49, 51, 56, 57, 64, 65, 67, 68, 70, 71, 72,
 74, 76, 78, 83, 85, 87, 89, 92, 95, 98, 99, 103, 108, 113, 118, 123,
 124, 127, 129, 130, 131, 132, 135, 137, 139, 141, 143, 148, 151, 153, 156, 158,
 160, 162, 164, 166, 167, 172, 173, 175, 177, 179);
end Css.Analysis.Parser.Parser_Shift_Reduce;
