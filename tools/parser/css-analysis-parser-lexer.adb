
with Ada.Text_IO;
with CSS.Analysis.Parser.Lexer_dfa;
with CSS.Analysis.Parser.Lexer_IO;
package body CSS.Analysis.Parser.Lexer is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada;
   use CSS.Analysis.Parser.Lexer_dfa;
   use CSS.Analysis.Parser.Lexer_IO;

   pragma Style_Checks (Off);
   pragma Warnings (Off);
   function YYLex return Token is
      subtype Short is Integer range -32768 .. 32767;
      yy_act : Integer;
      yy_c   : Short;

      --  returned upon end-of-file
      YY_END_TOK : constant Integer := 0;
      YY_END_OF_BUFFER : constant := 28;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
      yy_accept : constant array (0 .. 136) of Short :=
          (0,
        0,    0,   28,   26,   16,   16,   14,   11,   26,    6,
        7,   12,   13,   15,   26,   24,   26,   26,    8,   10,
        4,   26,    5,   26,   22,    2,    9,    3,   11,   26,
       26,   26,   20,   16,   19,    0,   22,   24,   17,    0,
        0,    0,   22,   22,    0,    0,   22,    0,   18,    0,
        1,    0,   20,    0,    0,    0,   20,   20,    0,   20,
        0,    0,    0,    0,    0,   23,    0,   22,   22,    0,
       25,   22,   22,   22,    0,    0,    0,    0,   21,    0,
       20,   20,   20,   20,   20,    0,    0,    0,    0,    0,
       22,   22,   22,    0,    0,    0,    0,    0,   20,   20,

       20,    0,    0,    0,   22,   22,    0,    0,    0,   20,
       20,    0,    0,   22,   22,    0,    0,   20,   20,    0,
        0,   22,   22,    0,    0,   20,   20,    0,    0,   22,
        0,    0,   20,    0,    0,    0
       );

      yy_ec : constant array (ASCII.NUL .. Character'Last) of Short := (0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    4,    5,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    6,    1,    7,    8,   36,    9,   37,   10,
       11,   12,   13,   14,   15,   39,   46,   16,   17,   18,
       19,   20,   21,   22,   23,   24,   25,   26,   57,   27,
       28,   29,   30,   62,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   46,   47,   48,   49,   50,   51,
       52,   53,   54,   55,   56,   57,   58,   59,   60,   61,
       31,   32,   33,   34,   35,   94,   36,   37,   38,   39,

       40,   41,   42,   43,   44,   45,   46,   47,   48,   49,
       50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
       60,   61,   62,   63,   64,   96,  126, others => 1

       );

      yy_meta : constant array (0 .. 64) of Short :=
          (0,
        1,    1,    2,    3,    3,    1,    1,    1,    1,    4,
        1,    1,    1,    1,    5,    6,    6,    6,    6,    6,
        6,    6,    6,    6,    6,    1,    1,    1,    7,    1,
        1,    8,    1,    1,    8,    8,    8,    8,    8,    8,
        8,    8,    8,    8,    8,    8,    8,    8,    8,    8,
        8,    8,    8,    8,    8,    8,    8,    8,    8,    8,
        8,    1,    1,    1
       );

      yy_base : constant array (0 .. 155) of Short :=
          (0,
        0,   58,  285, 1765,   64,   72, 1765, 1765,  275, 1765,
     1765, 1765, 1765, 1765,  231,    0,  234,   55, 1765, 1765,
     1765,  104, 1765,  235,  120, 1765,  161, 1765,  220,  162,
       56,  137,  136,   76, 1765,  163,  137,    0, 1765,   57,
      189,  138,  139,  229,  184,  179,  140,  255, 1765,  186,
     1765,  281,  183,  119,  307,  187,  185,  347,  128,  186,
      373,  211,  413,  122,  212, 1765,  459,  188,  485,  124,
      123, 1765,  225,  525,  226,  565,   81,  227, 1765,  611,
      278,  637, 1765,  228,  677,  279,  703,   62,  254,  743,
      769,  304,  795,  306,  821,   57,  305,  861,  887,  330,

      913,  939,  370,  965,  991, 1017, 1043,  371, 1069, 1095,
     1121, 1147, 1173, 1199, 1225, 1251, 1277, 1303, 1329, 1355,
     1381, 1407, 1433, 1459, 1485, 1511, 1537, 1563, 1589,  133,
      483, 1615,  161,  351,  635, 1765,   76,   77, 1652, 1660,
     1665, 1673,   74, 1677, 1685, 1690, 1698, 1703, 1711, 1719,
     1727, 1732, 1740, 1748, 1756
       );

      yy_def : constant array (0 .. 155) of Short :=
          (0,
      136,    1,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  137,  138,  136,  139,  136,  136,
      136,  140,  136,  136,  141,  136,  136,  136,  142,  143,
      144,  145,  146,  136,  136,  140,  141,  138,  136,  139,
      147,  148,  141,  141,  149,  136,  141,  150,  136,  142,
      136,  145,  146,  144,  151,  152,  146,  146,  136,  146,
      153,  148,  136,  136,  148,  136,  154,  141,   44,  149,
      149,  136,  141,   44,  152,  136,  136,  152,  136,  155,
      146,   58,  136,  146,   58,  148,   63,  136,  148,   63,
       44,  141,   74,  152,   76,  136,  152,   76,   58,  146,

       85,   63,  148,   90,   44,   74,   76,  152,   98,   58,
       85,   63,   90,   44,   74,   76,   98,   58,   85,   63,
       90,   44,   74,   76,   98,   58,   85,   63,   90,  122,
      152,   98,  126,  128,  152,    0,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136
       );

      yy_nxt : constant array (0 .. 1829) of Short :=
          (0,
        4,    5,    6,    5,    5,    7,    8,    4,    9,   10,
       11,   12,   13,   14,   15,   16,   16,   16,   16,   16,
       16,   16,   16,   16,   16,   17,   18,   19,    4,   20,
       21,   22,   23,   24,   25,   25,   25,   25,   25,   25,
       25,   25,   25,   25,   25,   25,   25,   25,   25,   25,
       25,   25,   25,   25,   25,   25,   25,   25,   25,   25,
       25,   26,   27,   28,   29,   34,   34,   34,   34,   40,
       54,  136,   30,   34,   34,   34,   34,   34,   34,   34,
       34,   53,   38,   37,   31,   79,   41,   55,   41,   32,
       66,   96,   33,   33,   33,   33,   33,   33,   33,   33,

       33,   33,   33,   33,   33,   33,   33,   33,   33,   33,
       33,   33,   33,   33,   33,   33,   33,   33,   33,   44,
       44,   44,   44,   44,   44,   44,   44,   44,   44,   46,
       71,   71,   88,  136,   73,   73,   73,   92,   83,   44,
       44,   44,   44,   44,   44,   59,   46,   64,   46,   46,
       55,   48,   58,   58,   58,   58,   58,   58,   58,   58,
       58,   58,   84,   84,   84,  100,   66,   61,   48,   67,
       48,   48,   58,   58,   58,   58,   58,   58,   44,   44,
       44,   44,   44,   44,   44,   44,   44,   44,   51,   72,
       43,   71,   59,   52,   59,   59,   77,   46,   44,   44,

       44,   44,   44,   44,   63,   63,   63,   63,   63,   63,
       63,   63,   63,   63,   61,   79,   61,   61,   80,   48,
       64,   64,   51,   49,   63,   63,   63,   63,   63,   63,
       43,   43,   43,   68,   46,   77,   77,   59,   46,   66,
       66,   45,   67,   67,   69,   69,   69,   69,   69,   69,
       69,   69,   69,   69,   79,   79,   48,   80,   80,   61,
       48,   39,   36,   64,   69,   69,   69,   69,   69,   69,
       74,   74,   74,   74,   74,   74,   74,   74,   74,   74,
       57,   62,   66,   35,  136,   67,  136,   59,   64,  136,
       74,   74,   74,   74,   74,   74,   58,   58,   58,   58,

       58,   58,   58,   58,   58,   58,   73,   66,   75,   61,
       67,  136,  136,   46,   77,   77,   58,   58,   58,   58,
       58,   58,   76,   76,   76,   76,   76,   76,   76,   76,
       76,   76,   84,   79,   79,   48,   80,   80,  136,   59,
      136,  136,   76,   76,   76,   76,   76,   76,   57,   57,
       57,   81,   89,   89,   89,  103,   59,  136,  136,  136,
      136,   61,   82,   82,   82,   82,   82,   82,   82,   82,
       82,   82,   89,   97,  136,  136,  136,  136,   61,   64,
       77,  136,   82,   82,   82,   82,   82,   82,   85,   85,
       85,   85,   85,   85,   85,   85,   85,   85,   66,   79,

      136,   67,   80,  136,  136,  136,  136,  136,   85,   85,
       85,   85,   85,   85,   62,   62,   62,   86,  136,  136,
      136,  136,   64,  136,  136,  136,  136,   65,   87,   87,
       87,   87,   87,   87,   87,   87,   87,   87,  136,  136,
      136,   66,  136,  136,   67,  136,  136,   65,   87,   87,
       87,   87,   87,   87,   65,   65,   65,   65,   65,   65,
       65,   65,   65,   65,   65,   65,   65,   65,   65,   65,
       65,   65,   65,   65,   90,   90,   90,   90,   90,   90,
       90,   90,   90,   90,   75,   75,   75,   94,  136,  136,
      136,  136,   77,  136,   90,   90,   90,   90,   90,   90,

       91,   91,   91,   91,   91,   91,   91,   91,   91,   91,
      136,   79,  136,  136,   80,  136,  136,  136,  136,  136,
       91,   91,   91,   91,   91,   91,   73,   73,   73,   92,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
       93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
       93,   93,   93,   93,   93,   93,   75,   75,   75,   94,
      136,  136,  136,  136,   77,  136,  136,  136,  136,   78,
       95,   95,   95,   95,   95,   95,   95,   95,   95,   95,
      136,  136,  136,   79,  136,  136,   80,  136,  136,   78,

       95,   95,   95,   95,   95,   95,   78,   78,   78,   78,
       78,   78,   78,   78,   78,   78,   78,   78,   78,   78,
       78,   78,   78,   78,   78,   78,   98,   98,   98,   98,
       98,   98,   98,   98,   98,   98,   97,   97,   97,  108,
      136,  136,  136,  136,   77,  136,   98,   98,   98,   98,
       98,   98,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,  136,   79,  136,  136,   80,  136,  136,  136,
      136,  136,   99,   99,   99,   99,   99,   99,   84,   84,
       84,  100,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  101,  101,  101,  101,  101,  101,  101,  101,

      101,  101,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  101,  101,  101,  101,  101,  101,  102,  102,
      102,  102,  102,  102,  102,  102,  102,  102,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  102,  102,
      102,  102,  102,  102,   89,   89,   89,  103,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  104,  104,
      104,  104,  104,  104,  104,  104,  104,  104,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  104,  104,
      104,  104,  104,  104,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  136,  136,  136,  136,  136,  136,

      136,  136,  136,  136,  105,  105,  105,  105,  105,  105,
      106,  106,  106,  106,  106,  106,  106,  106,  106,  106,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      106,  106,  106,  106,  106,  106,  107,  107,  107,  107,
      107,  107,  107,  107,  107,  107,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  107,  107,  107,  107,
      107,  107,   97,   97,   97,  108,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  109,  109,  109,  109,
      109,  109,  109,  109,  109,  109,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  109,  109,  109,  109,

      109,  109,  110,  110,  110,  110,  110,  110,  110,  110,
      110,  110,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  110,  110,  110,  110,  110,  110,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  111,  111,
      111,  111,  111,  111,  112,  112,  112,  112,  112,  112,
      112,  112,  112,  112,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  112,  112,  112,  112,  112,  112,
      113,  113,  113,  113,  113,  113,  113,  113,  113,  113,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,

      113,  113,  113,  113,  113,  113,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  114,  114,  114,  114,
      114,  114,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  115,  115,  115,  115,  115,  115,  116,  116,
      116,  116,  116,  116,  116,  116,  116,  116,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  116,  116,
      116,  116,  116,  116,  117,  117,  117,  117,  117,  117,
      117,  117,  117,  117,  136,  136,  136,  136,  136,  136,

      136,  136,  136,  136,  117,  117,  117,  117,  117,  117,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      118,  118,  118,  118,  118,  118,  119,  119,  119,  119,
      119,  119,  119,  119,  119,  119,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  119,  119,  119,  119,
      119,  119,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  120,  120,  120,  120,  120,  120,  121,  121,
      121,  121,  121,  121,  121,  121,  121,  121,  136,  136,

      136,  136,  136,  136,  136,  136,  136,  136,  121,  121,
      121,  121,  121,  121,  122,  122,  122,  122,  122,  122,
      122,  122,  122,  122,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  122,  122,  122,  122,  122,  122,
      123,  123,  123,  123,  123,  123,  123,  123,  123,  123,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      123,  123,  123,  123,  123,  123,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  124,  124,  124,  124,
      124,  124,  125,  125,  125,  125,  125,  125,  125,  125,

      125,  125,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  125,  125,  125,  125,  125,  125,  126,  126,
      126,  126,  126,  126,  126,  126,  126,  126,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  126,  126,
      126,  126,  126,  126,  127,  127,  127,  127,  127,  127,
      127,  127,  127,  127,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  127,  127,  127,  127,  127,  127,
      128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      128,  128,  128,  128,  128,  128,  129,  129,  129,  129,

      129,  129,  129,  129,  129,  129,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  129,  129,  129,  129,
      129,  129,   47,   47,   47,   47,   47,   47,   47,   47,
       47,   47,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,   47,   47,   47,   47,   47,   47,  130,  130,
      130,  130,  130,  130,  130,  130,  130,  130,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  130,  130,
      130,  130,  130,  130,  131,  131,  131,  131,  131,  131,
      131,  131,  131,  131,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  131,  131,  131,  131,  131,  131,

      132,  132,  132,  132,  132,  132,  132,  132,  132,  132,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      132,  132,  132,  132,  132,  132,   60,   60,   60,   60,
       60,   60,   60,   60,   60,   60,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,   60,   60,   60,   60,
       60,   60,  133,  133,  133,  133,  133,  133,  133,  133,
      133,  133,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  133,  133,  133,  133,  133,  133,   65,   65,
       65,   65,   65,   65,   65,   65,   65,   65,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,   65,   65,

       65,   65,   65,   65,  134,  134,  134,  134,  134,  134,
      134,  134,  134,  134,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  134,  134,  134,  134,  134,  134,
      135,  135,  135,  135,  135,  135,  135,  135,  135,  135,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      135,  135,  135,  135,  135,  135,   42,  136,  136,   42,
       43,  136,  136,   43,   43,   43,   43,   43,   47,   47,
       47,  136,   47,   50,   50,   50,   50,   50,   50,   50,
       50,   56,  136,  136,   56,   57,  136,  136,   57,   57,
       57,   57,   57,   60,   60,   60,  136,   60,   62,  136,

      136,   62,   62,   62,   62,   62,   65,   65,   65,   65,
       65,   70,  136,   70,   70,   70,   70,   70,   70,   73,
      136,  136,   73,   73,   73,   73,   73,   75,  136,  136,
       75,   75,   75,   75,   75,   78,   78,   78,   78,   78,
       84,  136,  136,   84,   84,   84,   84,   84,   89,  136,
      136,   89,   89,   89,   89,   89,   97,  136,  136,   97,
       97,   97,   97,   97,    3,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,

      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136
       );

      yy_chk : constant array (0 .. 1829) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    2,    5,    5,    5,    5,   18,
       31,   40,    2,    6,    6,    6,    6,   34,   34,   34,
       34,  143,  138,  137,    2,   96,   18,   31,   40,    2,
       88,   77,    2,    2,    2,    2,    2,    2,    2,    2,

        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,   22,
       22,   22,   22,   22,   22,   22,   22,   22,   22,   25,
       71,   70,   64,   54,  130,  130,  130,  130,   59,   22,
       22,   22,   22,   22,   22,   33,   37,   42,   43,   47,
       54,   25,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,  133,  133,  133,  133,   42,   33,   37,   42,
       43,   47,   32,   32,   32,   32,   32,   32,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   50,   46,
       68,   45,   53,   30,   57,   60,   56,   68,   36,   36,

       36,   36,   36,   36,   41,   41,   41,   41,   41,   41,
       41,   41,   41,   41,   53,   56,   57,   60,   56,   68,
       62,   65,   29,   27,   41,   41,   41,   41,   41,   41,
       44,   44,   44,   44,   73,   75,   78,   84,   44,   62,
       65,   24,   62,   65,   44,   44,   44,   44,   44,   44,
       44,   44,   44,   44,   75,   78,   73,   75,   78,   84,
       44,   17,   15,   89,   44,   44,   44,   44,   44,   44,
       48,   48,   48,   48,   48,   48,   48,   48,   48,   48,
       81,   86,   89,    9,    3,   89,    0,   81,   86,    0,
       48,   48,   48,   48,   48,   48,   52,   52,   52,   52,

       52,   52,   52,   52,   52,   52,   92,   86,   94,   81,
       86,    0,    0,   92,   97,   94,   52,   52,   52,   52,
       52,   52,   55,   55,   55,   55,   55,   55,   55,   55,
       55,   55,  100,   97,   94,   92,   97,   94,    0,  100,
        0,    0,   55,   55,   55,   55,   55,   55,   58,   58,
       58,   58,  134,  134,  134,  134,   58,    0,    0,    0,
        0,  100,   58,   58,   58,   58,   58,   58,   58,   58,
       58,   58,  103,  108,    0,    0,    0,    0,   58,  103,
      108,    0,   58,   58,   58,   58,   58,   58,   61,   61,
       61,   61,   61,   61,   61,   61,   61,   61,  103,  108,

        0,  103,  108,    0,    0,    0,    0,    0,   61,   61,
       61,   61,   61,   61,   63,   63,   63,   63,    0,    0,
        0,    0,   63,    0,    0,    0,    0,   63,   63,   63,
       63,   63,   63,   63,   63,   63,   63,   63,    0,    0,
        0,   63,    0,    0,   63,    0,    0,   63,   63,   63,
       63,   63,   63,   63,   63,   63,   63,   63,   63,   63,
       63,   63,   63,   63,   63,   63,   63,   63,   63,   63,
       63,   63,   63,   63,   67,   67,   67,   67,   67,   67,
       67,   67,   67,   67,  131,  131,  131,  131,    0,    0,
        0,    0,  131,    0,   67,   67,   67,   67,   67,   67,

       69,   69,   69,   69,   69,   69,   69,   69,   69,   69,
        0,  131,    0,    0,  131,    0,    0,    0,    0,    0,
       69,   69,   69,   69,   69,   69,   74,   74,   74,   74,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       74,   74,   74,   74,   74,   74,   74,   74,   74,   74,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       74,   74,   74,   74,   74,   74,   76,   76,   76,   76,
        0,    0,    0,    0,   76,    0,    0,    0,    0,   76,
       76,   76,   76,   76,   76,   76,   76,   76,   76,   76,
        0,    0,    0,   76,    0,    0,   76,    0,    0,   76,

       76,   76,   76,   76,   76,   76,   76,   76,   76,   76,
       76,   76,   76,   76,   76,   76,   76,   76,   76,   76,
       76,   76,   76,   76,   76,   76,   80,   80,   80,   80,
       80,   80,   80,   80,   80,   80,  135,  135,  135,  135,
        0,    0,    0,    0,  135,    0,   80,   80,   80,   80,
       80,   80,   82,   82,   82,   82,   82,   82,   82,   82,
       82,   82,    0,  135,    0,    0,  135,    0,    0,    0,
        0,    0,   82,   82,   82,   82,   82,   82,   85,   85,
       85,   85,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,   85,   85,   85,   85,   85,   85,   85,   85,

       85,   85,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,   85,   85,   85,   85,   85,   85,   87,   87,
       87,   87,   87,   87,   87,   87,   87,   87,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,   87,   87,
       87,   87,   87,   87,   90,   90,   90,   90,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,   90,   90,
       90,   90,   90,   90,   90,   90,   90,   90,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,   90,   90,
       90,   90,   90,   90,   91,   91,   91,   91,   91,   91,
       91,   91,   91,   91,    0,    0,    0,    0,    0,    0,

        0,    0,    0,    0,   91,   91,   91,   91,   91,   91,
       93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       93,   93,   93,   93,   93,   93,   95,   95,   95,   95,
       95,   95,   95,   95,   95,   95,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,   95,   95,   95,   95,
       95,   95,   98,   98,   98,   98,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,   98,   98,   98,   98,
       98,   98,   98,   98,   98,   98,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,   98,   98,   98,   98,

       98,   98,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,   99,   99,   99,   99,   99,   99,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  101,  101,
      101,  101,  101,  101,  102,  102,  102,  102,  102,  102,
      102,  102,  102,  102,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  102,  102,  102,  102,  102,  102,
      104,  104,  104,  104,  104,  104,  104,  104,  104,  104,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,

      104,  104,  104,  104,  104,  104,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,  105,  105,  105,  105,
      105,  105,  106,  106,  106,  106,  106,  106,  106,  106,
      106,  106,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  106,  106,  106,  106,  106,  106,  107,  107,
      107,  107,  107,  107,  107,  107,  107,  107,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  107,  107,
      107,  107,  107,  107,  109,  109,  109,  109,  109,  109,
      109,  109,  109,  109,    0,    0,    0,    0,    0,    0,

        0,    0,    0,    0,  109,  109,  109,  109,  109,  109,
      110,  110,  110,  110,  110,  110,  110,  110,  110,  110,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      110,  110,  110,  110,  110,  110,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  111,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,  111,  111,  111,  111,
      111,  111,  112,  112,  112,  112,  112,  112,  112,  112,
      112,  112,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  112,  112,  112,  112,  112,  112,  113,  113,
      113,  113,  113,  113,  113,  113,  113,  113,    0,    0,

        0,    0,    0,    0,    0,    0,    0,    0,  113,  113,
      113,  113,  113,  113,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  114,  114,  114,  114,  114,  114,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      115,  115,  115,  115,  115,  115,  116,  116,  116,  116,
      116,  116,  116,  116,  116,  116,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,  116,  116,  116,  116,
      116,  116,  117,  117,  117,  117,  117,  117,  117,  117,

      117,  117,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  117,  117,  117,  117,  117,  117,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  118,  118,
      118,  118,  118,  118,  119,  119,  119,  119,  119,  119,
      119,  119,  119,  119,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  119,  119,  119,  119,  119,  119,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      120,  120,  120,  120,  120,  120,  121,  121,  121,  121,

      121,  121,  121,  121,  121,  121,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,  121,  121,  121,  121,
      121,  121,  122,  122,  122,  122,  122,  122,  122,  122,
      122,  122,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  122,  122,  122,  122,  122,  122,  123,  123,
      123,  123,  123,  123,  123,  123,  123,  123,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  123,  123,
      123,  123,  123,  123,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  124,  124,  124,  124,  124,  124,

      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      125,  125,  125,  125,  125,  125,  126,  126,  126,  126,
      126,  126,  126,  126,  126,  126,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,  126,  126,  126,  126,
      126,  126,  127,  127,  127,  127,  127,  127,  127,  127,
      127,  127,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  127,  127,  127,  127,  127,  127,  128,  128,
      128,  128,  128,  128,  128,  128,  128,  128,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,  128,  128,

      128,  128,  128,  128,  129,  129,  129,  129,  129,  129,
      129,  129,  129,  129,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,  129,  129,  129,  129,  129,  129,
      132,  132,  132,  132,  132,  132,  132,  132,  132,  132,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
      132,  132,  132,  132,  132,  132,  139,    0,    0,  139,
      140,    0,    0,  140,  140,  140,  140,  140,  141,  141,
      141,    0,  141,  142,  142,  142,  142,  142,  142,  142,
      142,  144,    0,    0,  144,  145,    0,    0,  145,  145,
      145,  145,  145,  146,  146,  146,    0,  146,  147,    0,

        0,  147,  147,  147,  147,  147,  148,  148,  148,  148,
      148,  149,    0,  149,  149,  149,  149,  149,  149,  150,
        0,    0,  150,  150,  150,  150,  150,  151,    0,    0,
      151,  151,  151,  151,  151,  152,  152,  152,  152,  152,
      153,    0,    0,  153,  153,  153,  153,  153,  154,    0,
        0,  154,  154,  154,  154,  154,  155,    0,    0,  155,
      155,  155,  155,  155,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,

      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136,  136,
      136,  136,  136,  136,  136,  136,  136,  136,  136
       );


      --  copy whatever the last rule matched to the standard output


      --  enter a start condition.
      --  Using procedure requires a () after the ENTER, but makes everything
      --  much neater.

      procedure ENTER (state : Integer) is
      begin
         yy_start := 1 + 2 * state;
      end ENTER;

      --  action number for EOF rule of a given start state
      function YY_STATE_EOF (state : Integer) return Integer is
      begin
         return YY_END_OF_BUFFER + state + 1;
      end YY_STATE_EOF;

      --  return all but the first 'n' matched characters back to the input stream
      procedure yyless (n : Integer) is
      begin
         yy_ch_buf (yy_cp) := yy_hold_char; --  undo effects of setting up yytext
         yy_cp := yy_bp + n;
         yy_c_buf_p := yy_cp;
         YY_DO_BEFORE_ACTION; -- set up yytext again
      end yyless;

      --  redefine this if you have something you want each time.
      procedure YY_USER_ACTION is
      begin
         null;
      end YY_USER_ACTION;

      --  yy_get_previous_state - get the state just before the EOB char was reached

      function yy_get_previous_state return yy_state_type is
         yy_current_state : yy_state_type;
         yy_c : Short;
         yy_bp : constant Integer := yytext_ptr;
      begin
         yy_current_state := yy_start;
         if yy_ch_buf (yy_bp - 1) = ASCII.LF then
            yy_current_state := yy_current_state + 1;
         end if;

         for yy_cp in yytext_ptr .. yy_c_buf_p - 1 loop
            yy_c := yy_ec (yy_ch_buf (yy_cp));
            if yy_accept (yy_current_state) /= 0 then
               yy_last_accepting_state := yy_current_state;
               yy_last_accepting_cpos := yy_cp;
               yy_last_yylineno := yylineno;
               yy_last_yylinecol := yylinecol;
            end if;
            while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
               yy_current_state := yy_def (yy_current_state);
               if yy_current_state >= 137 then
                  yy_c := yy_meta (yy_c);
               end if;
            end loop;
            yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
         end loop;

         return yy_current_state;
      end yy_get_previous_state;

      procedure yyrestart (input_file : File_Type) is
      begin
         Open_Input (Text_IO.Name (input_file));
      end yyrestart;

   begin -- of YYLex
      <<new_file>>
      --  this is where we enter upon encountering an end-of-file and
      --  yyWrap () indicating that we should continue processing

      if yy_init then
         if yy_start = 0 then
            yy_start := 1;      -- first start state
         end if;

         --  we put in the '\n' and start reading from [1] so that an
         --  initial match-at-newline will be true.

         yy_ch_buf (0) := ASCII.LF;
         yy_n_chars := 1;

         --  we always need two end-of-buffer characters. The first causes
         --  a transition to the end-of-buffer state. The second causes
         --  a jam in that state.

         yy_ch_buf (yy_n_chars) := YY_END_OF_BUFFER_CHAR;
         yy_ch_buf (yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

         yy_eof_has_been_seen := False;

         yytext_ptr := 1;
         yy_c_buf_p := yytext_ptr;
         yy_hold_char := yy_ch_buf (yy_c_buf_p);
         yy_init := False;
      end if; -- yy_init

      loop                -- loops until end-of-file is reached


         yy_cp := yy_c_buf_p;

         --  support of yytext
         yy_ch_buf (yy_cp) := yy_hold_char;

         --  yy_bp points to the position in yy_ch_buf of the start of the
         --  current run.
         yy_bp := yy_cp;
         yy_current_state := yy_start;
         if yy_ch_buf (yy_bp - 1) = ASCII.LF then
            yy_current_state := yy_current_state + 1;
         end if;
         loop
               yy_c := yy_ec (yy_ch_buf (yy_cp));
               if yy_accept (yy_current_state) /= 0 then
                  yy_last_accepting_state := yy_current_state;
                  yy_last_accepting_cpos := yy_cp;
                  yy_last_yylineno := yylineno;
                  yy_last_yylinecol := yylinecol;
               end if;
               while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
                  yy_current_state := yy_def (yy_current_state);
                  if yy_current_state >= 137 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            if yy_ch_buf (yy_cp) = ASCII.LF then
               yylineno := yylineno + 1;
               yylinecol := 1;
            else
               yylinecol := yylinecol + 1;
            end if;
            yy_cp := yy_cp + 1;
            if yy_current_state = 136 then
                exit;
            end if;
         end loop;
         yy_cp := yy_last_accepting_cpos;
         yy_current_state := yy_last_accepting_state;
         yylineno := yy_last_yylineno;
         yylinecol := yy_last_yylinecol;

   <<next_action>>
         yy_act := yy_accept (yy_current_state);
         YY_DO_BEFORE_ACTION;
         YY_USER_ACTION;

         if aflex_debug then  -- output acceptance info. for (-d) debug mode
            Text_IO.Put (Standard_Error, "  -- Aflex.YYLex accept rule #");
            Text_IO.Put (Standard_Error, Integer'Image (yy_act));
            Text_IO.Put_Line (Standard_Error, "(""" & YYText & """)");
         end if;


   <<do_action>>   -- this label is used only to access EOF actions
         case yy_act is
            when 0 => -- must backtrack
            -- undo the effects of YY_DO_BEFORE_ACTION
            yy_ch_buf (yy_cp) := yy_hold_char;
            yy_cp := yy_last_accepting_cpos;
            yylineno := yy_last_yylineno;
            yylinecol := yy_last_yylinecol;
            yy_current_state := yy_last_accepting_state;
            goto next_action;



         when 1 => 
            yy_ch_buf (yy_cp) := yy_hold_char; -- undo effects of setting up yytext
            yy_cp := yy_cp - 1;
            yy_c_buf_p := yy_cp;
            YY_DO_BEFORE_ACTION; -- set up yytext again
            null;

         when 2 => 
            return '{';

         when 3 => 
            return '}';

         when 4 => 
            return '[';

         when 5 => 
            return ']';

         when 6 => 
            return '(';

         when 7 => 
            return ')';

         when 8 => 
            return '=';

         when 9 => 
            return '|';

         when 10 => 
            return '?';

         when 11 => 
            return '#';

         when 12 => 
            return '*';

         when 13 => 
            return '+';

         when 14 => 
            return '!';

         when 15 => 
            return ',';

         when 16 => 
            return S;

         when 17 => 
             return R_DEFINE; 

         when 18 => 
             return R_FOLLOW; 

         when 19 => 
             return R_ANY; 

         when 20 => 
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_PROPERTY; 

         when 21 => 
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_DEF_NAME; 

         when 22 => 
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_IDENT; 

         when 23 => 
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NAME; 

         when 24 => 
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NUM; 

         when 25 => 
             null; 

         when 26 => 
             Error (yylineno, yylinecol, "Illegal character '" & YYText & "'"); 

         when 27 => 
                        raise AFLEX_SCANNER_JAMMED;
         when YY_END_OF_BUFFER + INITIAL + 1 =>
               return End_Of_Input;
            when YY_END_OF_BUFFER =>
               --  undo the effects of YY_DO_BEFORE_ACTION
               yy_ch_buf (yy_cp) := yy_hold_char;

               yytext_ptr := yy_bp;

               case yy_get_next_buffer is
                  when EOB_ACT_END_OF_FILE =>
                     if yyWrap then
                        --  note: because we've taken care in
                        --  yy_get_next_buffer() to have set up yytext,
                        --  we can now set up yy_c_buf_p so that if some
                        --  total hoser (like aflex itself) wants
                        --  to call the scanner after we return the
                        --  End_Of_Input, it'll still work - another
                        --  End_Of_Input will get returned.

                        yy_c_buf_p := yytext_ptr;

                        yy_act := YY_STATE_EOF ((yy_start - 1) / 2);

                        goto do_action;
                     else
                        --  start processing a new file
                        yy_init := True;
                        goto new_file;
                     end if;

                  when EOB_ACT_RESTART_SCAN =>
                     yy_c_buf_p := yytext_ptr;
                     yy_hold_char := yy_ch_buf (yy_c_buf_p);

                  when EOB_ACT_LAST_MATCH =>
                     yy_c_buf_p := yy_n_chars;
                     yy_current_state := yy_get_previous_state;

                     yy_cp := yy_c_buf_p;
                     yy_bp := yytext_ptr;
                     goto next_action;
                  when others =>
                     null;
               end case; --  case yy_get_next_buffer()

            when others =>
               Text_IO.Put ("action # ");
               Text_IO.Put (Integer'Image (yy_act));
               Text_IO.New_Line;
               raise AFLEX_INTERNAL_ERROR;
         end case; --  case (yy_act)
      end loop; --  end of loop waiting for end of file
   end YYLex;
   pragma Style_Checks (On);

end CSS.Analysis.Parser.Lexer;



