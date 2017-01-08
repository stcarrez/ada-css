pragma Style_Checks (Off);
package Css.Parser.Parser_Shift_Reduce is

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
,( 2, 5),( 3, 6),( 4, 7),(-1,-6)
-- State  1
,( 0,-3001),( 2, 5),( 3, 6),( 4, 7),(-1,-6)
-- State  2
,(-1,-2)
-- State  3
,( 27, 12),( 30, 10),(-1,-3000)
-- State  4
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-5)
-- State  5
,( 3, 17),( 4, 18),(-1,-13)
-- State  6
,(-1,-14)
-- State  7
,(-1,-15)
-- State  8
,(-1,-3000)
-- State  9
,(-1,-1)
-- State  10
,( 1, 21),( 2, 23),(-1,-3000)
-- State  11
,(-1,-3)
-- State  12
,( 2, 5),( 3, 6),( 4, 7),(-1,-3000)
-- State  13
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-10)
-- State  14
,( 2, 5),( 3, 6),( 4, 7),(-1,-3000)
-- State  15
,( 2, 5),( 3, 6),( 4, 7),(-1,-3000)
-- State  16
,( 2, 5),( 3, 6),( 4, 7),(-1,-3000)
-- State  17
,(-1,-11)
-- State  18
,(-1,-12)
-- State  19
,( 2, 23),( 31, 28),(-1,-3000)
-- State  20
,( 32, 30),(-1,-3000)
-- State  21
,( 31, 31),( 32, 32),(-1,-3000)
-- State  22
,( 33, 33),(-1,-3000)
-- State  23
,(-1,-52)
-- State  24
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-4)
-- State  25
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-7)
-- State  26
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-8)
-- State  27
,( 2, 5),( 3, 6),( 4, 7),( 26, 14),( 28, 15),( 29, 16),(-1,-9)
-- State  28
,(-1,-16)
-- State  29
,( 32, 34),(-1,-3000)
-- State  30
,(-1,-19)
-- State  31
,(-1,-17)
-- State  32
,(-1,-20)
-- State  33
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),( 22, 37),(-1,-3000)
-- State  34
,(-1,-18)
-- State  35
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),( 22, 37),(-1,-22)
-- State  36
,(-1,-25)
-- State  37
,( 14, 54),(-1,-3000)
-- State  38
,(-1,-26)
-- State  39
,(-1,-27)
-- State  40
,(-1,-28)
-- State  41
,(-1,-29)
-- State  42
,(-1,-30)
-- State  43
,(-1,-31)
-- State  44
,(-1,-32)
-- State  45
,(-1,-33)
-- State  46
,(-1,-34)
-- State  47
,( 9, 55),(-1,-3000)
-- State  48
,( 5, 60),( 7, 61),( 11, 48),(-1,-3000)
-- State  49
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),(-1,-3000)
-- State  50
,( 2, 64),(-1,-3000)
-- State  51
,(-1,-23)
-- State  52
,(-1,-21)
-- State  53
,(-1,-24)
-- State  54
,(-1,-53)
-- State  55
,( 16, 65),(-1,-3000)
-- State  56
,( 1, 67),( 16, 66),( 29, 68),( 34, 69),(-1,-3000)
-- State  57
,( 28, 70),( 35, 71),(-1,-40)
-- State  58
,(-1,-43)
-- State  59
,(-1,-44)
-- State  60
,(-1,-45)
-- State  61
,(-1,-46)
-- State  62
,( 16, 72),( 27, 73),(-1,-3000)
-- State  63
,(-1,-49)
-- State  64
,( 7, 74),(-1,-3000)
-- State  65
,(-1,-35)
-- State  66
,(-1,-36)
-- State  67
,(-1,-37)
-- State  68
,( 5, 60),( 7, 61),( 11, 48),(-1,-3000)
-- State  69
,( 5, 60),( 7, 61),( 11, 48),(-1,-3000)
-- State  70
,( 5, 60),( 7, 61),( 11, 48),(-1,-3000)
-- State  71
,( 5, 60),( 7, 61),( 11, 48),(-1,-3000)
-- State  72
,(-1,-47)
-- State  73
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),(-1,-3000)
-- State  74
,( 16, 81),( 27, 80),(-1,-3000)
-- State  75
,( 28, 70),( 35, 71),(-1,-38)
-- State  76
,( 28, 70),( 35, 71),(-1,-39)
-- State  77
,(-1,-41)
-- State  78
,(-1,-42)
-- State  79
,(-1,-48)
-- State  80
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),(-1,-3000)
-- State  81
,(-1,-51)
-- State  82
,( 2, 45),( 5, 41),( 6, 42),( 7, 43),( 8, 44),( 9, 46),( 10, 47),( 11, 48),( 12, 49),( 13, 50),( 16, 83),(-1,-3000)
-- State  83
,(-1,-50)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 83) of Integer :=
( 0,
 4, 9, 10, 13, 20, 23, 24, 25, 26, 27, 30, 31, 35, 42, 46, 50,
 54, 55, 56, 59, 61, 64, 66, 67, 74, 81, 88, 95, 96, 98, 99, 100,
 101, 113, 114, 126, 127, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 140,
 144, 155, 157, 158, 159, 160, 161, 163, 168, 171, 172, 173, 174, 175, 178, 179,
 181, 182, 183, 184, 188, 192, 196, 200, 201, 212, 215, 218, 221, 222, 223, 224,
 235, 236, 248);
end Css.Parser.Parser_Shift_Reduce;