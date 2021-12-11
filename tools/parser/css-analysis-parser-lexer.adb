
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
      YY_END_OF_BUFFER : constant := 29;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
      INITIAL : constant := 0;
      yy_accept : constant array (0 .. 137) of Short :=
          (0,
        0,    0,   29,   27,   17,   17,   14,   11,   27,    6,
        7,   12,   13,   15,   27,   16,   25,   27,   27,    8,
       10,    4,   27,    5,   27,   23,    2,    9,    3,   11,
       27,   27,   27,   21,   17,   20,    0,   23,   25,   18,
        0,    0,    0,   23,   23,    0,    0,   23,    0,   19,
        0,    1,    0,   21,    0,    0,    0,   21,   21,    0,
       21,    0,    0,    0,    0,    0,   24,    0,   23,   23,
        0,   26,   23,   23,   23,    0,    0,    0,    0,   22,
        0,   21,   21,   21,   21,   21,    0,    0,    0,    0,
        0,   23,   23,   23,    0,    0,    0,    0,    0,   21,

       21,   21,    0,    0,    0,   23,   23,    0,    0,    0,
       21,   21,    0,    0,   23,   23,    0,    0,   21,   21,
        0,    0,   23,   23,    0,    0,   21,   21,    0,    0,
       23,    0,    0,   21,    0,    0,    0
       );

      yy_ec : constant array (ASCII.NUL .. Character'Last) of Short := (0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    4,    5,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    6,    1,    7,    8,    1,    9,    1,   10,
       11,   12,   13,   14,   15,    1,   16,   17,   17,   17,
       17,   17,   17,   17,   17,   17,   17,   18,    1,   19,
       20,   21,   22,    1,   28,   28,   28,   28,   28,   28,
       27,   27,   27,   27,   27,   27,   27,   27,   27,   27,
       27,   27,   27,   27,   27,   27,   27,   27,   27,   27,
       23,   24,   25,   26,   27,    1,   28,   28,   28,   28,

       28,   28,   27,   27,   27,   27,   27,   27,   27,   27,
       27,   27,   27,   27,   27,   27,   27,   27,   27,   27,
       27,   27,   29,   30,   31,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,

       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32, others => 1

       );

      yy_meta : constant array (0 .. 32) of Short :=
          (0,
        1,    1,    2,    3,    3,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1
       );

      yy_base : constant array (0 .. 147) of Short :=
          (0,
        0,   26,  265,  427,   32,   44,  427,  427,  255,  427,
      427,  427,  427,  427,   28,  427,  246,  242,   46,  427,
      427,  427,   23,  427,  254,   69,  427,  230,  427,  236,
       35,   87,   40,  110,   78,  427,   47,    0,  217,  427,
       63,   48,  133,    0,  164,  223,  206,    0,   49,  427,
      209,  427,   71,    0,   76,   77,  187,    0,  218,  192,
        0,   81,    0,  249,  189,    0,  427,   89,  190,   90,
      177,  172,  427,    0,  276,    0,  307,  166,    0,  427,
       93,  155,   95,  427,    0,  334,  146,   96,  111,    0,
      365,  105,  128,  111,  123,  112,   95,    0,  392,  113,

       89,  118,  119,   86,  127,  128,  134,  135,   82,  136,
      142,  154,  155,  156,  158,  159,  161,  173,  177,  178,
      181,  182,  190,  196,  199,  208,  209,  212,  213,  215,
      221,  227,  230,  231,  239,  240,  427,   71,  420,   70,
       68,  423,   43,   42,   41,   38,   37
       );

      yy_def : constant array (0 .. 147) of Short :=
          (0,
      137,    1,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  138,  137,  137,  137,  137,  137,  137,  139,
      137,  137,  140,  137,  137,  137,  138,   26,  137,  137,
      137,  141,  137,   26,  137,  142,  137,   26,  143,  137,
      139,  137,  140,   34,  137,  144,  137,   34,  137,  137,
       34,  145,   43,   43,  137,   43,  137,  146,   26,   45,
      142,  142,  137,   26,  137,   57,   57,  137,   57,  137,
      147,   34,   59,  137,   34,  137,   43,   64,  137,   43,
       43,   45,   26,   75,   57,   77,  137,   57,   57,   59,

       34,   86,   64,   43,   91,   45,   75,   77,   57,   99,
       59,   86,   64,   91,   45,   75,   77,   99,   59,   86,
       64,   91,   45,   75,   77,   99,   59,   86,   64,   91,
       75,   77,   99,   86,   91,   99,    0,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137
       );

      yy_nxt : constant array (0 .. 459) of Short :=
          (0,
        4,    5,    6,    5,    5,    7,    8,    4,    9,   10,
       11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
        4,   21,   22,   23,   24,   25,   26,   26,   27,   28,
       29,   26,   30,   35,   35,   35,   35,   98,   90,   45,
       31,   85,   76,   74,   32,   35,   35,   35,   35,   33,
       45,   37,   34,   34,   38,   38,   59,   34,   53,   38,
       41,   54,   54,   45,   64,   75,   54,   59,   63,   42,
       58,   44,   43,   43,   45,   64,   75,   43,   47,   35,
       35,   35,   35,   48,   98,   48,   42,   59,   90,   43,
       43,   85,   49,   77,   43,   48,   48,   86,   59,   56,

       48,   55,   57,   57,   77,   91,   92,   57,   86,   99,
       56,  100,  103,   57,   57,   80,   91,   92,   57,   60,
       99,  106,  100,  103,   61,   76,   61,  107,  108,  111,
       74,   67,  106,   62,  112,  113,   61,   61,  107,  108,
      111,   61,   65,  114,  115,  112,  113,   66,   63,   66,
      116,  117,  118,   67,  114,  115,   68,   58,  119,   66,
       66,  116,  117,  118,   66,   44,   44,   44,   69,  119,
      120,  121,  122,   47,  123,  124,   97,  125,   48,   72,
       70,  120,  121,  122,   72,  123,  124,   49,  125,  126,
       48,   70,   44,  127,  128,   48,   78,  129,  130,   89,

      126,   79,   84,   79,  127,  128,   48,   80,  129,  130,
       81,   52,  131,   79,   79,  132,   73,   48,   79,   58,
       58,   58,   82,  131,  133,   61,  132,   60,  134,   66,
       72,  135,   61,   39,   83,  133,   61,   48,   52,  134,
       66,   62,  135,   79,   61,   83,  136,   61,   48,   61,
       63,   63,   63,   87,   79,   66,   79,  136,   61,   50,
       46,   40,   39,   36,  137,   88,   66,   79,  137,  137,
      137,  137,  137,  137,  137,  137,   88,   74,   74,   74,
       93,  137,  137,  137,  137,   47,  137,  137,  137,  137,
       48,  137,   94,  137,  137,  137,  137,  137,  137,   49,

      137,  137,   48,   94,  137,  137,  137,   48,   76,   76,
       76,   95,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,   96,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,   96,   85,   85,   85,  101,  137,
      137,  137,  137,   60,  137,  137,  137,  137,   61,  137,
      102,  137,  137,  137,  137,  137,  137,   62,  137,  137,
       61,  102,  137,  137,  137,   61,   90,   90,   90,  104,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  105,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  105,   98,   98,   98,  109,  137,  137,  137,

      137,  137,  137,  137,  137,  137,  137,  137,  110,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  110,
       51,   51,   51,   71,  137,   71,    3,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137
       );

      yy_chk : constant array (0 .. 459) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    2,    5,    5,    5,    5,  147,  146,   23,
        2,  145,  144,  143,    2,    6,    6,    6,    6,    2,
       23,   15,    2,    2,   15,   15,   33,    2,   31,   15,
       19,   31,   31,   37,   42,   49,   31,   33,  141,   19,
      140,  138,   19,   19,   37,   42,   49,   19,   26,   35,
       35,   35,   35,   26,  109,   26,   41,   53,  104,   41,
       41,  101,   26,   56,   41,   26,   26,   62,   53,   55,

       26,   32,   55,   55,   56,   68,   70,   55,   62,   81,
       32,   83,   88,   32,   32,   97,   68,   70,   32,   34,
       81,   92,   83,   88,   34,   95,   34,   94,   96,  100,
       93,   89,   92,   34,  102,  103,   34,   34,   94,   96,
      100,   34,   43,  105,  106,  102,  103,   43,   87,   43,
      107,  108,  110,   43,  105,  106,   43,   82,  111,   43,
       43,  107,  108,  110,   43,   45,   45,   45,   45,  111,
      112,  113,  114,   45,  115,  116,   78,  117,   45,   72,
       45,  112,  113,  114,   71,  115,  116,   45,  117,  118,
       45,   45,   69,  119,  120,   45,   57,  121,  122,   65,

      118,   57,   60,   57,  119,  120,  123,   57,  121,  122,
       57,   51,  124,   57,   57,  125,   47,  123,   57,   59,
       59,   59,   59,  124,  126,  127,  125,   59,  128,  129,
       46,  130,   59,   39,   59,  126,  127,  131,   30,  128,
      129,   59,  130,  132,   59,   59,  133,  134,  131,   59,
       64,   64,   64,   64,  132,  135,  136,  133,  134,   28,
       25,   18,   17,    9,    3,   64,  135,  136,    0,    0,
        0,    0,    0,    0,    0,    0,   64,   75,   75,   75,
       75,    0,    0,    0,    0,   75,    0,    0,    0,    0,
       75,    0,   75,    0,    0,    0,    0,    0,    0,   75,

        0,    0,   75,   75,    0,    0,    0,   75,   77,   77,
       77,   77,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,   77,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,   77,   86,   86,   86,   86,    0,
        0,    0,    0,   86,    0,    0,    0,    0,   86,    0,
       86,    0,    0,    0,    0,    0,    0,   86,    0,    0,
       86,   86,    0,    0,    0,   86,   91,   91,   91,   91,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,   91,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,   91,   99,   99,   99,   99,    0,    0,    0,

        0,    0,    0,    0,    0,    0,    0,    0,   99,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,   99,
      139,  139,  139,  142,    0,  142,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137
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
               if yy_current_state >= 138 then
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
                  if yy_current_state >= 138 then
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
            if yy_current_state = 137 then
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
            return '/';

         when 17 =>
            return S;

         when 18 =>
             return R_DEFINE; 

         when 19 =>
             return R_FOLLOW; 

         when 20 =>
             return R_ANY; 

         when 21 =>
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_PROPERTY; 

         when 22 =>
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_DEF_NAME; 

         when 23 =>
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_IDENT; 

         when 24 =>
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NAME; 

         when 25 =>
             Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NUM; 

         when 26 =>
             null; 

         when 27 =>
             Error (yylineno, yylinecol, "Illegal character '" & YYText & "'"); 

         when 28 =>
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



