private package CSS.Parser.Parser_Shift_Reduce is

   type Small_Integer is range -32_000 .. 32_000;

   type Shift_Reduce_Entry is record
      T   : Small_Integer;
      Act : Small_Integer;
   end record;
   pragma Pack (Shift_Reduce_Entry);

   type Row is new Integer range -1 .. Integer'Last;

   --  pragma suppress(index_check);

   type Shift_Reduce_Array is array (Row range <>) of Shift_Reduce_Entry;

   Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
      ((-1, -1) --  Dummy Entry

      --  State  0
      , (1, 3), (3, 33), (4, 41), (5, 21), (25, 36), (29, 39), (40, 16)
      , (41, 19), (43, 28), (44, 23), (45, 24), (46, 32), (50, 27), (51, 22)
      , (64, 40), (68, 43), (69, 42), (-1, -25)
      --  State  1
      , (1, 45), (3, 33), (4, 41), (5, 21), (25, 36), (29, 39), (40, 16)
      , (41, 19), (43, 28), (44, 23), (45, 24), (46, 32), (50, 27), (51, 22)
      , (64, 40), (68, 43), (69, 42), (-1, -1)
      --  State  2
      , (-1, -3)
      --  State  3
      , (61, 47), (62, 48), (-1, -4)
      --  State  4
      , (-1, -5)
      --  State  5
      , (-1, -6)
      --  State  6
      , (-1, -7)
      --  State  7
      , (-1, -8)
      --  State  8
      , (-1, -9)
      --  State  9
      , (-1, -10)
      --  State  10
      , (43, 28), (44, 23), (45, 24), (-1, -11)
      --  State  11
      , (43, 28), (-1, -25)
      --  State  12
      , (61, 51), (-1, -3000)
      --  State  13
      , (43, 28), (-1, -25)
      --  State  14
      , (43, 28), (-1, -25)
      --  State  15
      , (61, 54), (-1, -3000)
      --  State  16
      , (11, 55), (-1, -3000)
      --  State  17
      , (-1, -19)
      --  State  18
      , (61, 56), (63, 57), (-1, -3000)
      --  State  19
      , (43, 28), (-1, -25)
      --  State  20
      , (43, 28), (-1, -25)
      --  State  21
      , (1, 61), (43, 28), (-1, -25)
      --  State  22
      , (43, 28), (-1, -25)
      --  State  23
      , (-1, -20)
      --  State  24
      , (-1, -21)
      --  State  25
      , (43, 63), (-1, -22)
      --  State  26
      , (3, 33), (4, 41), (25, 36), (29, 39), (35, 66), (38, 65), (46, 32)
      , (64, 40), (66, 64), (68, 43), (69, 42), (-1, -113)
      --  State  27
      , (-1, -90)
      --  State  28
      , (-1, -24)
      --  State  29
      , (3, 33), (4, 41), (25, 36), (29, 39), (43, 28), (46, 32), (64, 40)
      , (68, 43), (69, 42), (-1, -25)
      --  State  30
      , (-1, -118)
      --  State  31
      , (-1, -119)
      --  State  32
      , (-1, -120)
      --  State  33
      , (-1, -121)
      --  State  34
      , (-1, -122)
      --  State  35
      , (-1, -123)
      --  State  36
      , (3, 73), (4, 41), (29, 39), (46, 72), (64, 40), (68, 43), (69, 42)
      , (-1, -3000)
      --  State  37
      , (4, 77), (69, 78), (-1, -3000)
      --  State  38
      , (-1, -129)
      --  State  39
      , (1, 81), (43, 28), (-1, -25)
      --  State  40
      , (4, 83), (47, 84), (64, 82), (-1, -3000)
      --  State  41
      , (68, 85), (-1, -130)
      --  State  42
      , (68, 86), (-1, -131)
      --  State  43
      , (-1, -127)
      --  State  44
      , (0, -3001), (-1, -3000)
      --  State  45
      , (61, 47), (62, 48), (-1, -3000)
      --  State  46
      , (-1, -2)
      --  State  47
      , (43, 28), (-1, -25)
      --  State  48
      , (43, 28), (-1, -25)
      --  State  49
      , (-1, -18)
      --  State  50
      , (1, 93), (4, 94), (43, 63), (62, 97), (69, 95), (-1, -3000)
      --  State  51
      , (43, 28), (-1, -25)
      --  State  52
      , (43, 63), (61, 99), (-1, -3000)
      --  State  53
      , (1, 104), (3, 33), (4, 41), (8, 105), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (25, 36), (29, 39), (43, 63), (46, 32), (48, 110)
      , (52, 106), (64, 40), (68, 43), (69, 42), (-1, -3000)
      --  State  54
      , (43, 28), (-1, -25)
      --  State  55
      , (60, 114), (-1, -3000)
      --  State  56
      , (-1, -110)
      --  State  57
      , (43, 28), (-1, -25)
      --  State  58
      , (4, 125), (23, 129), (25, 123), (26, 124), (27, 128), (43, 63), (-1, -3000)
      --  State  59
      , (4, 133), (43, 63), (64, 132), (-1, -93)
      --  State  60
      , (4, 137), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (43, 63), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  61
      , (61, 148), (-1, -3000)
      --  State  62
      , (43, 63), (-1, -48)
      --  State  63
      , (-1, -23)
      --  State  64
      , (43, 28), (-1, -25)
      --  State  65
      , (43, 28), (-1, -25)
      --  State  66
      , (43, 28), (-1, -25)
      --  State  67
      , (3, 33), (4, 41), (25, 36), (29, 39), (43, 28), (46, 32), (64, 40)
      , (68, 43), (69, 42), (-1, -25)
      --  State  68
      , (3, 33), (4, 41), (25, 36), (29, 39), (46, 32), (64, 40), (68, 43)
      , (69, 42), (-1, -3000)
      --  State  69
      , (43, 63), (-1, -116)
      --  State  70
      , (-1, -117)
      --  State  71
      , (-1, -132)
      --  State  72
      , (-1, -133)
      --  State  73
      , (-1, -134)
      --  State  74
      , (-1, -135)
      --  State  75
      , (-1, -136)
      --  State  76
      , (43, 28), (-1, -25)
      --  State  77
      , (-1, -130)
      --  State  78
      , (-1, -131)
      --  State  79
      , (-1, -128)
      --  State  80
      , (4, 155), (43, 63), (-1, -3000)
      --  State  81
      , (30, 156), (-1, -3000)
      --  State  82
      , (4, 157), (-1, -3000)
      --  State  83
      , (-1, -148)
      --  State  84
      , (43, 28), (-1, -25)
      --  State  85
      , (-1, -125)
      --  State  86
      , (-1, -126)
      --  State  87
      , (-1, -3000)
      --  State  88
      , (1, 161), (3, 33), (4, 162), (25, 36), (29, 39), (43, 63), (46, 32)
      , (64, 40), (68, 43), (69, 163), (-1, -36)
      --  State  89
      , (43, 63), (-1, -107)
      --  State  90
      , (1, 167), (60, 166), (-1, -109)
      --  State  91
      , (43, 28), (-1, -25)
      --  State  92
      , (1, 170), (64, 169), (-1, -3000)
      --  State  93
      , (62, 171), (-1, -167)
      --  State  94
      , (43, 28), (-1, -25)
      --  State  95
      , (4, 173), (-1, -3000)
      --  State  96
      , (62, 174), (-1, -3000)
      --  State  97
      , (43, 28), (-1, -25)
      --  State  98
      , (1, 104), (3, 33), (4, 41), (25, 36), (29, 39), (43, 63), (46, 32)
      , (64, 40), (68, 43), (69, 42), (-1, -36)
      --  State  99
      , (43, 28), (-1, -25)
      --  State  100
      , (1, 104), (3, 33), (4, 41), (8, 105), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (25, 36), (29, 39), (46, 32), (48, 110), (52, 106)
      , (62, 178), (64, 40), (68, 43), (69, 42), (-1, -3000)
      --  State  101
      , (-1, -43)
      --  State  102
      , (-1, -44)
      --  State  103
      , (61, 180), (-1, -3000)
      --  State  104
      , (61, 181), (62, 48), (-1, -3000)
      --  State  105
      , (43, 28), (-1, -25)
      --  State  106
      , (43, 28), (-1, -25)
      --  State  107
      , (43, 28), (-1, -25)
      --  State  108
      , (43, 28), (-1, -25)
      --  State  109
      , (43, 28), (-1, -25)
      --  State  110
      , (43, 28), (-1, -25)
      --  State  111
      , (43, 28), (-1, -25)
      --  State  112
      , (43, 28), (-1, -25)
      --  State  113
      , (1, 190), (4, 94), (43, 63), (69, 95), (-1, -3000)
      --  State  114
      , (43, 28), (-1, -25)
      --  State  115
      , (3, 33), (4, 41), (25, 36), (29, 39), (43, 63), (46, 32), (64, 40)
      , (68, 43), (69, 42), (-1, -3000)
      --  State  116
      , (63, 194), (-1, -37)
      --  State  117
      , (-1, -50)
      --  State  118
      , (-1, -51)
      --  State  119
      , (-1, -52)
      --  State  120
      , (24, 196), (-1, -60)
      --  State  121
      , (24, 197), (-1, -61)
      --  State  122
      , (23, 129), (-1, -62)
      --  State  123
      , (43, 28), (-1, -25)
      --  State  124
      , (43, 28), (-1, -25)
      --  State  125
      , (43, 28), (-1, -25)
      --  State  126
      , (-1, -66)
      --  State  127
      , (-1, -69)
      --  State  128
      , (1, 204), (43, 28), (-1, -25)
      --  State  129
      , (27, 128), (-1, -3000)
      --  State  130
      , (64, 132), (-1, -89)
      --  State  131
      , (-1, -92)
      --  State  132
      , (4, 207), (-1, -3000)
      --  State  133
      , (64, 132), (-1, -93)
      --  State  134
      , (4, 137), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (46, 146), (47, 147), (48, 110), (52, 106), (56, 145)
      , (61, 210), (63, 211), (66, 138), (67, 139), (-1, -3000)
      --  State  135
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (46, 146), (47, 147), (48, 110), (52, 106), (56, 145)
      , (66, 212), (67, 213), (-1, -176)
      --  State  136
      , (-1, -180)
      --  State  137
      , (43, 28), (-1, -25)
      --  State  138
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (48, 110)
      , (52, 106), (-1, -3000)
      --  State  139
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (48, 110)
      , (52, 106), (-1, -3000)
      --  State  140
      , (-1, -187)
      --  State  141
      , (43, 28), (-1, -25)
      --  State  142
      , (43, 28), (-1, -25)
      --  State  143
      , (-1, -191)
      --  State  144
      , (-1, -192)
      --  State  145
      , (-1, -193)
      --  State  146
      , (43, 28), (-1, -25)
      --  State  147
      , (1, 223), (43, 28), (-1, -25)
      --  State  148
      , (-1, -41)
      --  State  149
      , (43, 63), (-1, -99)
      --  State  150
      , (43, 63), (-1, -100)
      --  State  151
      , (43, 63), (-1, -101)
      --  State  152
      , (43, 63), (-1, -115)
      --  State  153
      , (3, 33), (4, 41), (25, 36), (29, 39), (43, 28), (46, 32), (64, 40)
      , (68, 43), (69, 42), (-1, -25)
      --  State  154
      , (28, 225), (43, 63), (-1, -3000)
      --  State  155
      , (43, 28), (-1, -25)
      --  State  156
      , (-1, -140)
      --  State  157
      , (-1, -147)
      --  State  158
      , (4, 230), (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112)
      , (43, 63), (48, 110), (52, 106), (-1, -3000)
      --  State  159
      , (1, 104), (3, 33), (4, 41), (25, 36), (29, 39), (46, 32), (62, 231)
      , (64, 40), (68, 43), (69, 42), (-1, -3000)
      --  State  160
      , (-1, -35)
      --  State  161
      , (61, 181), (62, 48), (-1, -167)
      --  State  162
      , (3, -130), (4, -130), (25, -130), (29, -130), (35, -130), (38, -130), (43, 28)
      , (46, -130), (61, -130), (63, -130), (66, -130), (68, 85), (69, -130), (-1, -25)
      --  State  163
      , (4, 173), (68, 86), (-1, -131)
      --  State  164
      , (62, 233), (-1, -3000)
      --  State  165
      , (1, 190), (4, 94), (60, 235), (69, 95), (-1, -108)
      --  State  166
      , (43, 28), (-1, -25)
      --  State  167
      , (60, 166), (-1, -3000)
      --  State  168
      , (43, 63), (-1, -159)
      --  State  169
      , (1, 239), (43, 28), (-1, -25)
      --  State  170
      , (-1, -166)
      --  State  171
      , (43, 28), (-1, -25)
      --  State  172
      , (43, 63), (-1, -168)
      --  State  173
      , (43, 28), (-1, -25)
      --  State  174
      , (43, 28), (-1, -25)
      --  State  175
      , (43, 63), (-1, -106)
      --  State  176
      , (1, 104), (3, 33), (4, 41), (25, 36), (29, 39), (46, 32), (62, 243)
      , (64, 40), (68, 43), (69, 42), (-1, -3000)
      --  State  177
      , (1, 190), (4, 94), (43, 63), (69, 95), (-1, -3000)
      --  State  178
      , (43, 28), (-1, -25)
      --  State  179
      , (-1, -42)
      --  State  180
      , (43, 28), (-1, -25)
      --  State  181
      , (-1, -111)
      --  State  182
      , (43, 63), (-1, -194)
      --  State  183
      , (43, 63), (-1, -195)
      --  State  184
      , (43, 63), (-1, -196)
      --  State  185
      , (43, 63), (-1, -197)
      --  State  186
      , (43, 63), (-1, -198)
      --  State  187
      , (43, 63), (-1, -199)
      --  State  188
      , (43, 63), (-1, -200)
      --  State  189
      , (43, 63), (-1, -201)
      --  State  190
      , (-1, -167)
      --  State  191
      , (62, 248), (-1, -3000)
      --  State  192
      , (43, 63), (-1, -15)
      --  State  193
      , (3, 33), (4, 41), (25, 36), (29, 39), (35, 66), (38, 65), (46, 32)
      , (64, 40), (66, 64), (68, 43), (69, 42), (-1, -112)
      --  State  194
      , (43, 28), (-1, -25)
      --  State  195
      , (24, 196), (-1, -53)
      --  State  196
      , (43, 28), (-1, -25)
      --  State  197
      , (43, 28), (-1, -25)
      --  State  198
      , (-1, -65)
      --  State  199
      , (-1, -68)
      --  State  200
      , (4, 254), (27, 128), (43, 63), (-1, -3000)
      --  State  201
      , (4, 255), (43, 63), (-1, -3000)
      --  State  202
      , (24, 196), (43, 63), (-1, -60)
      --  State  203
      , (4, 257), (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112)
      , (23, 129), (27, 128), (43, 63), (48, 110), (52, 106), (-1, -3000)
      --  State  204
      , (28, 261), (-1, -3000)
      --  State  205
      , (-1, -70)
      --  State  206
      , (-1, -91)
      --  State  207
      , (-1, -94)
      --  State  208
      , (64, 132), (-1, -88)
      --  State  209
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (46, 146), (47, 147), (48, 110), (52, 106), (56, 145)
      , (66, 212), (67, 213), (-1, -175)
      --  State  210
      , (-1, -40)
      --  State  211
      , (43, 28), (-1, -25)
      --  State  212
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 28)
      , (48, 110), (52, 106), (-1, -25)
      --  State  213
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 28)
      , (48, 110), (52, 106), (-1, -25)
      --  State  214
      , (43, 28), (-1, -25)
      --  State  215
      , (-1, -179)
      --  State  216
      , (32, 266), (43, 63), (-1, -189)
      --  State  217
      , (-1, -185)
      --  State  218
      , (-1, -186)
      --  State  219
      , (43, 63), (-1, -188)
      --  State  220
      , (43, 63), (-1, -190)
      --  State  221
      , (43, 63), (-1, -202)
      --  State  222
      , (4, 137), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (28, 268), (43, 63), (46, 146), (47, 147), (48, 110)
      , (52, 106), (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  223
      , (28, 269), (-1, -3000)
      --  State  224
      , (43, 63), (-1, -114)
      --  State  225
      , (-1, -124)
      --  State  226
      , (30, 276), (32, 270), (43, 63), (49, 271), (53, 272), (57, 273), (58, 274)
      , (59, 275), (-1, -3000)
      --  State  227
      , (4, 279), (28, 278), (66, 280), (-1, -3000)
      --  State  228
      , (-1, -152)
      --  State  229
      , (-1, -153)
      --  State  230
      , (43, 28), (-1, -25)
      --  State  231
      , (43, 28), (-1, -25)
      --  State  232
      , (-1, -34)
      --  State  233
      , (43, 28), (-1, -25)
      --  State  234
      , (43, 28), (-1, -25)
      --  State  235
      , (43, 28), (-1, -25)
      --  State  236
      , (43, 63), (-1, -161)
      --  State  237
      , (1, 190), (4, 94), (60, 235), (69, 95), (-1, -158)
      --  State  238
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (43, 63), (46, 146), (47, 147), (48, 110), (52, 106)
      , (55, 289), (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  239
      , (-1, -165)
      --  State  240
      , (43, 63), (-1, -105)
      --  State  241
      , (43, 63), (-1, -169)
      --  State  242
      , (43, 63), (-1, -104)
      --  State  243
      , (43, 28), (-1, -25)
      --  State  244
      , (60, 291), (62, 292), (-1, -3000)
      --  State  245
      , (43, 28), (-1, -25)
      --  State  246
      , (43, 63), (-1, -39)
      --  State  247
      , (1, 190), (4, 94), (43, 63), (69, 95), (-1, -3000)
      --  State  248
      , (43, 28), (-1, -25)
      --  State  249
      , (4, 125), (23, 129), (25, 123), (26, 124), (27, 128), (43, 63), (-1, -3000)
      --  State  250
      , (24, 196), (-1, -58)
      --  State  251
      , (27, 128), (43, 63), (-1, -3000)
      --  State  252
      , (27, 128), (43, 63), (-1, -3000)
      --  State  253
      , (-1, -54)
      --  State  254
      , (43, 28), (-1, -25)
      --  State  255
      , (43, 28), (-1, -25)
      --  State  256
      , (24, 196), (-1, -57)
      --  State  257
      , (43, 28), (-1, -25)
      --  State  258
      , (43, 28), (-1, -25)
      --  State  259
      , (43, 28), (-1, -25)
      --  State  260
      , (43, 28), (-1, -25)
      --  State  261
      , (43, 28), (-1, -25)
      --  State  262
      , (4, 137), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (43, 63), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  263
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (43, 63), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  264
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (43, 63), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (66, 138), (67, 139), (-1, -3000)
      --  State  265
      , (43, 63), (-1, -189)
      --  State  266
      , (43, 28), (-1, -25)
      --  State  267
      , (4, 137), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (28, 311), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (63, 211), (66, 138), (67, 139), (-1, -3000)
      --  State  268
      , (43, 28), (-1, -25)
      --  State  269
      , (43, 28), (-1, -25)
      --  State  270
      , (-1, -141)
      --  State  271
      , (-1, -142)
      --  State  272
      , (-1, -143)
      --  State  273
      , (-1, -144)
      --  State  274
      , (-1, -145)
      --  State  275
      , (-1, -146)
      --  State  276
      , (-1, -137)
      --  State  277
      , (43, 28), (-1, -25)
      --  State  278
      , (-1, -149)
      --  State  279
      , (43, 28), (-1, -25)
      --  State  280
      , (4, 230), (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112)
      , (48, 110), (52, 106), (-1, -3000)
      --  State  281
      , (32, 317), (43, 63), (-1, -155)
      --  State  282
      , (43, 63), (-1, -33)
      --  State  283
      , (43, 63), (-1, -47)
      --  State  284
      , (43, 63), (-1, -156)
      --  State  285
      , (43, 63), (-1, -160)
      --  State  286
      , (43, 28), (-1, -25)
      --  State  287
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (22, 319), (46, 146), (47, 147), (48, 110), (52, 106)
      , (56, 145), (63, 321), (65, 320), (66, 138), (67, 139), (-1, -163)
      --  State  288
      , (-1, -184)
      --  State  289
      , (-1, -164)
      --  State  290
      , (43, 63), (-1, -32)
      --  State  291
      , (43, 28), (-1, -25)
      --  State  292
      , (43, 28), (-1, -25)
      --  State  293
      , (43, 63), (-1, -96)
      --  State  294
      , (62, 327), (-1, -3000)
      --  State  295
      , (43, 63), (-1, -46)
      --  State  296
      , (-1, -49)
      --  State  297
      , (27, 128), (-1, -64)
      --  State  298
      , (-1, -59)
      --  State  299
      , (-1, -67)
      --  State  300
      , (43, 63), (-1, -55)
      --  State  301
      , (43, 63), (-1, -56)
      --  State  302
      , (28, 331), (37, 329), (38, 330), (43, 63), (64, 333), (-1, -3000)
      --  State  303
      , (37, 329), (38, 330), (43, 63), (-1, -3000)
      --  State  304
      , (28, 335), (43, 63), (-1, -3000)
      --  State  305
      , (28, 336), (43, 63), (-1, -3000)
      --  State  306
      , (43, 63), (-1, -74)
      --  State  307
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (46, 146), (47, 147), (48, 110), (52, 106), (56, 145)
      , (66, 212), (67, 213), (-1, -174)
      --  State  308
      , (-1, -177)
      --  State  309
      , (-1, -178)
      --  State  310
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 63)
      , (48, 110), (52, 106), (-1, -3000)
      --  State  311
      , (43, 28), (-1, -25)
      --  State  312
      , (43, 63), (-1, -172)
      --  State  313
      , (43, 63), (-1, -173)
      --  State  314
      , (4, 339), (11, 340), (43, 63), (-1, -3000)
      --  State  315
      , (43, 63), (-1, -150)
      --  State  316
      , (-1, -151)
      --  State  317
      , (43, 28), (-1, -25)
      --  State  318
      , (43, 63), (-1, -157)
      --  State  319
      , (43, 28), (-1, -25)
      --  State  320
      , (43, 28), (-1, -25)
      --  State  321
      , (43, 28), (-1, -25)
      --  State  322
      , (-1, -162)
      --  State  323
      , (4, 214), (8, 105), (11, 141), (13, 142), (14, 107), (15, 108), (16, 109)
      , (17, 111), (18, 112), (46, 146), (47, 147), (48, 110), (52, 106), (56, 145)
      , (66, 138), (67, 139), (-1, -3000)
      --  State  324
      , (-1, -183)
      --  State  325
      , (1, 190), (4, 94), (43, 63), (62, 346), (69, 95), (-1, -3000)
      --  State  326
      , (43, 63), (-1, -87)
      --  State  327
      , (43, 28), (-1, -25)
      --  State  328
      , (24, 197), (-1, -63)
      --  State  329
      , (32, 349), (-1, -78)
      --  State  330
      , (32, 350), (-1, -77)
      --  State  331
      , (43, 28), (-1, -25)
      --  State  332
      , (43, 28), (-1, -25)
      --  State  333
      , (43, 28), (-1, -25)
      --  State  334
      , (43, 28), (-1, -25)
      --  State  335
      , (43, 28), (-1, -25)
      --  State  336
      , (43, 28), (-1, -25)
      --  State  337
      , (43, 28), (-1, -25)
      --  State  338
      , (43, 63), (-1, -171)
      --  State  339
      , (43, 28), (-1, -25)
      --  State  340
      , (43, 28), (-1, -25)
      --  State  341
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 63)
      , (48, 110), (52, 106), (-1, -3000)
      --  State  342
      , (43, 63), (-1, -170)
      --  State  343
      , (43, 63), (-1, -97)
      --  State  344
      , (43, 63), (-1, -98)
      --  State  345
      , (-1, -182)
      --  State  346
      , (43, 28), (-1, -25)
      --  State  347
      , (43, 28), (-1, -25)
      --  State  348
      , (43, 63), (-1, -45)
      --  State  349
      , (-1, -75)
      --  State  350
      , (-1, -76)
      --  State  351
      , (43, 63), (-1, -73)
      --  State  352
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 63)
      , (48, 110), (52, 106), (-1, -3000)
      --  State  353
      , (4, 365), (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112)
      , (43, 63), (48, 110), (52, 106), (-1, -3000)
      --  State  354
      , (4, 367), (43, 63), (-1, -3000)
      --  State  355
      , (43, 63), (-1, -71)
      --  State  356
      , (43, 63), (-1, -72)
      --  State  357
      , (43, 63), (-1, -181)
      --  State  358
      , (30, 368), (43, 63), (-1, -3000)
      --  State  359
      , (30, 369), (43, 63), (-1, -3000)
      --  State  360
      , (-1, -154)
      --  State  361
      , (43, 63), (-1, -86)
      --  State  362
      , (43, 63), (-1, -95)
      --  State  363
      , (-1, -79)
      --  State  364
      , (-1, -84)
      --  State  365
      , (-1, -85)
      --  State  366
      , (-1, -82)
      --  State  367
      , (28, -81), (43, 28), (-1, -25)
      --  State  368
      , (-1, -138)
      --  State  369
      , (-1, -139)
      --  State  370
      , (8, 105), (14, 107), (15, 108), (16, 109), (17, 111), (18, 112), (43, 63)
      , (48, 110), (52, 106), (-1, -3000)
      --  State  371
      , (-1, -80)
      );

   --  The offset vector
   Shift_Reduce_Offset : constant array (0 .. 371) of Row :=
      (0,
       18, 36, 37, 40, 41, 42, 43, 44, 45, 46,
       50, 52, 54, 56, 58, 60, 62, 63, 66, 68,
       70, 73, 75, 76, 77, 79, 91, 92, 93, 103,
       104, 105, 106, 107, 108, 109, 117, 120, 121, 124,
       128, 130, 132, 133, 135, 138, 139, 141, 143, 144,
       150, 152, 155, 174, 176, 178, 179, 181, 188, 192,
       210, 212, 214, 215, 217, 219, 221, 231, 240, 242,
       243, 244, 245, 246, 247, 248, 250, 251, 252, 253,
       256, 258, 260, 261, 263, 264, 265, 266, 277, 279,
       282, 284, 287, 289, 291, 293, 295, 297, 308, 310,
       329, 330, 331, 333, 336, 338, 340, 342, 344, 346,
       348, 350, 352, 357, 359, 369, 371, 372, 373, 374,
       376, 378, 380, 382, 384, 386, 387, 388, 391, 393,
       395, 396, 398, 400, 419, 436, 437, 439, 448, 457,
       458, 460, 462, 463, 464, 465, 467, 470, 471, 473,
       475, 477, 479, 489, 492, 494, 495, 496, 507, 518,
       519, 522, 536, 539, 541, 546, 548, 550, 552, 555,
       556, 558, 560, 562, 564, 566, 577, 582, 584, 585,
       587, 588, 590, 592, 594, 596, 598, 600, 602, 604,
       605, 607, 609, 621, 623, 625, 627, 629, 630, 631,
       635, 638, 641, 654, 656, 657, 658, 659, 661, 678,
       679, 681, 691, 701, 703, 704, 707, 708, 709, 711,
       713, 715, 734, 736, 738, 739, 748, 752, 753, 754,
       756, 758, 759, 761, 763, 765, 767, 772, 791, 792,
       794, 796, 798, 800, 803, 805, 807, 812, 814, 821,
       823, 826, 829, 830, 832, 834, 836, 838, 840, 842,
       844, 846, 864, 882, 900, 902, 904, 923, 925, 927,
       928, 929, 930, 931, 932, 933, 934, 936, 937, 939,
       949, 952, 954, 956, 958, 960, 962, 982, 983, 984,
       986, 988, 990, 992, 994, 996, 997, 999, 1000, 1001,
       1003, 1005, 1011, 1015, 1018, 1021, 1023, 1040, 1041, 1042,
       1052, 1054, 1056, 1058, 1062, 1064, 1065, 1067, 1069, 1071,
       1073, 1075, 1076, 1093, 1094, 1100, 1102, 1104, 1106, 1108,
       1110, 1112, 1114, 1116, 1118, 1120, 1122, 1124, 1126, 1128,
       1130, 1140, 1142, 1144, 1146, 1147, 1149, 1151, 1153, 1154,
       1155, 1157, 1167, 1178, 1181, 1183, 1185, 1187, 1190, 1193,
       1194, 1196, 1198, 1199, 1200, 1201, 1202, 1205, 1206, 1207,
       1217);

end CSS.Parser.Parser_Shift_Reduce;
