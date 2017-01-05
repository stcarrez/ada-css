
with CSS.Core;
with Ada.Text_IO;
with CSS.Parser.Lexer_dfa;
with CSS.Parser.Lexer_IO;

package body CSS.Parser.Lexer is

   use Ada.Text_IO;
   use Ada;
   use CSS.Parser.Lexer_dfa;
   use CSS.Parser.Lexer_IO;

   --  Line_Number : Natural := 0;

   pragma Style_Checks (Off);
   pragma Warnings (Off);
   function YYLex return Token is
      subtype Short is Integer range -32768 .. 32767;
      yy_act : Integer;
      yy_c   : Short;

      --  returned upon end-of-file
      YY_END_TOK : constant Integer := 0;
      YY_END_OF_BUFFER : constant := 53;
      subtype yy_state_type is Integer;
      yy_current_state : yy_state_type;
INITIAL : constant := 0;
COMMENT : constant := 1;
QUOTE : constant := 2;
      yy_accept : constant array (0 .. 96) of Short :=
          (0,
        0,    0,   29,   29,    0,    0,   53,   51,    1,    3,
        2,   16,   24,   51,    8,    9,   20,   23,   12,   22,
       13,   21,   36,   36,    4,   14,   17,   19,   18,    5,
       50,   10,   51,   11,   50,   50,   50,    6,    7,   15,
       29,   31,   30,   25,   27,   26,   41,    1,   33,   33,
       49,   48,   28,   37,    0,   36,    0,    0,    0,   50,
       41,   35,   50,   50,   50,   29,   30,   30,   32,    0,
       33,   33,   49,   33,   48,   43,    0,   38,   39,   34,
       50,   50,   50,   50,   42,   38,   42,   50,   50,   50,
       46,   45,   50,   50,   47,    0

       );

      yy_ec : constant array (ASCII.NUL .. Character'Last) of Short := (0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    4,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    5,    6,    7,    1,    8,    1,    1,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   18,   18,
       18,   18,   18,   18,   18,   19,   19,   20,   21,   22,
       23,   24,    1,   25,   26,   26,   26,   26,   26,   26,
       27,   27,   27,   27,   27,   27,   27,   27,   27,   27,
       27,   27,   27,   27,   27,   27,   27,   27,   27,   27,
       28,   29,   30,    1,   27,    1,   31,   26,   32,   26,

       33,   26,   34,   27,   27,   27,   27,   35,   36,   27,
       37,   38,   27,   39,   27,   40,   27,   27,   27,   41,
       27,   27,   42,    1,   43,   44,    1, others => 1

       );

      yy_meta : constant array (0 .. 44) of Short :=
          (0,
        1,    1,    2,    1,    1,    1,    1,    1,    1,    1,
        3,    1,    1,    4,    1,    1,    5,    5,    5,    1,
        1,    1,    1,    1,    1,    5,    6,    1,    1,    1,
        5,    5,    5,    6,    6,    6,    6,    6,    6,    6,
        6,    1,    1,    1
       );

      yy_base : constant array (0 .. 105) of Short :=
          (0,
        0,    0,   42,   43,   44,   45,  223,   38,   47,  284,
       41,   44,   50,   53,   56,   59,   64,   78,   81,   84,
       87,   96,  108,  181,   91,  100,  103,  111,  114,  117,
      120,  125,  133,  136,  139,  128,  108,  143,  146,  152,
        0,  284,  156,  284,  284,  284,  156,  219,  173,  193,
      218,    0,  284,  284,  159,  147,  182,  176,    0,    0,
      107,  164,  176,  180,  180,    0,  182,  184,  284,  177,
        0,  173,    0,    0,    0,  167,  172,  284,  284,    0,
      167,  163,  169,  162,  284,  284,    0,  185,  178,  131,
      284,  284,   91,  103,  284,  284,  251,  257,  259,  262,

      268,  274,  277,  106,   47
       );

      yy_def : constant array (0 .. 105) of Short :=
          (0,
       96,    1,   97,   97,   98,   98,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       99,   96,   96,   23,   96,   96,   96,   96,   96,   96,
      100,   96,   96,   96,  100,   35,   35,   96,   96,   96,
      101,   96,  102,   96,   96,   96,   96,   96,   96,   96,
       50,  103,   96,   96,   96,   23,   96,   96,  104,  100,
       35,   96,  100,  100,  100,  101,  102,  102,   96,   96,
      105,  105,   51,   50,  103,   96,   96,   96,   96,  104,
      100,  100,  100,  100,   96,   96,  100,  100,  100,  100,
       96,   96,  100,  100,   96,    0,   96,   96,   96,   96,

       96,   96,   96,   96,   96
       );

      yy_nxt : constant array (0 .. 328) of Short :=
          (0,
        8,    9,   10,   11,   12,   13,   14,    8,   15,   16,
       17,   18,   19,   20,   21,   22,   23,   24,   24,   25,
       26,   27,   28,   29,   30,   31,   31,   32,   33,   34,
       35,   36,   31,   31,   31,   31,   31,   31,   31,   37,
       31,   38,   39,   40,   42,   42,   45,   45,   48,   46,
       46,   71,   43,   43,   47,   47,   47,   47,   47,   47,
       47,   47,   47,   47,   47,   47,   47,   47,   47,   49,
       49,   49,   47,   47,   47,   47,   47,   47,   50,   51,
       47,   47,   47,   50,   50,   50,   51,   51,   51,   51,
       51,   51,   51,   51,   47,   47,   47,   47,   47,   47,

       47,   47,   47,   47,   47,   47,   53,   47,   47,   47,
       80,   95,   47,   47,   47,   54,   47,   47,   47,   47,
       47,   47,   55,   94,   56,   56,   56,   47,   47,   47,
       47,   47,   47,   47,   47,   47,   61,   61,   61,   81,
       57,   47,   47,   47,   65,   58,   60,   60,   59,   62,
       62,   47,   47,   47,   47,   61,   61,   61,   64,   47,
       47,   47,   47,   47,   47,   93,   68,   60,   47,   47,
       47,   69,   47,   47,   47,   76,   76,   76,   63,   77,
       62,   62,   47,   76,   76,   76,   92,   96,   70,   49,
       49,   49,   96,   91,   68,   90,   70,   96,   71,   69,

       89,   88,   87,   71,   71,   72,   73,   86,   85,   74,
       74,   74,   85,   84,   83,   82,   79,   78,   74,   73,
       48,   96,   96,   74,   74,   74,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   96,   96,   96,
       96,   96,   96,   73,   96,   96,   96,   96,   73,   73,
       73,   41,   41,   41,   41,   41,   41,   44,   44,   44,
       44,   44,   44,   52,   52,   60,   60,   60,   66,   96,
       96,   66,   66,   66,   67,   96,   67,   67,   67,   67,
       75,   75,   75,    7,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,

       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96
       );

      yy_chk : constant array (0 .. 328) of Short :=
          (0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    3,    4,    5,    6,    9,    5,
        6,  105,    3,    4,    8,    8,    8,   11,   11,   11,
       12,   12,   12,    9,    9,    9,   13,   13,   13,   14,
       14,   14,   15,   15,   15,   16,   16,   16,   14,   14,
       17,   17,   17,   14,   14,   14,   14,   14,   14,   14,
       14,   14,   14,   14,   18,   18,   18,   19,   19,   19,

       20,   20,   20,   21,   21,   21,   22,   25,   25,   25,
      104,   94,   22,   22,   22,   23,   26,   26,   26,   27,
       27,   27,   23,   93,   23,   23,   23,   28,   28,   28,
       29,   29,   29,   30,   30,   30,   31,   31,   31,   61,
       23,   32,   32,   32,   37,   23,   61,   37,   23,   33,
       33,   33,   34,   34,   34,   35,   35,   35,   36,   38,
       38,   38,   39,   39,   39,   90,   43,   36,   40,   40,
       40,   43,   47,   47,   47,   55,   55,   55,   35,   56,
       62,   62,   62,   76,   76,   76,   89,   56,   47,   49,
       49,   49,   67,   88,   68,   84,   62,   67,   49,   68,

       83,   82,   81,   49,   49,   49,   50,   77,   72,   50,
       50,   50,   70,   65,   64,   63,   58,   57,   50,   50,
       48,   24,    7,   50,   50,   50,   50,   50,   50,   50,
       50,   50,   50,   50,   51,   51,   51,    0,    0,    0,
        0,    0,    0,   51,    0,    0,    0,    0,   51,   51,
       51,   97,   97,   97,   97,   97,   97,   98,   98,   98,
       98,   98,   98,   99,   99,  100,  100,  100,  101,    0,
        0,  101,  101,  101,  102,    0,  102,  102,  102,  102,
      103,  103,  103,   96,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,

       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96
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
      begin
         yy_current_state := yy_start;

         for yy_cp in yytext_ptr .. yy_c_buf_p - 1 loop
            yy_c := yy_ec (yy_ch_buf (yy_cp));
            if yy_accept (yy_current_state) /= 0 then
               yy_last_accepting_state := yy_current_state;
               yy_last_accepting_cpos := yy_cp;
            end if;
            while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
               yy_current_state := yy_def (yy_current_state);
               if yy_current_state >= 97 then
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
         loop
               yy_c := yy_ec (yy_ch_buf (yy_cp));
               if yy_accept (yy_current_state) /= 0 then
                  yy_last_accepting_state := yy_current_state;
                  yy_last_accepting_cpos := yy_cp;
               end if;
               while yy_chk (yy_base (yy_current_state) + yy_c) /= yy_current_state loop
                  yy_current_state := yy_def (yy_current_state);
                  if yy_current_state >= 97 then
                     yy_c := yy_meta (yy_c);
                  end if;
               end loop;
               yy_current_state := yy_nxt (yy_base (yy_current_state) + yy_c);
            yy_cp := yy_cp + 1;
            if yy_current_state = 96 then
                exit;
            end if;
         end loop;
         yy_cp := yy_last_accepting_cpos;
         yy_current_state := yy_last_accepting_state;

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
            yy_current_state := yy_last_accepting_state;
            goto next_action;



         when 1 => 
             null; 

         when 2 => 
            null;

         when 3 => 
            Line_Number := Line_Number + 1;

         when 4 => 
            return ':';

         when 5 => 
            return '@';

         when 6 => 
            return '{';

         when 7 => 
            return '}';

         when 8 => 
            return '(';

         when 9 => 
            return ')';

         when 10 => 
            return '[';

         when 11 => 
            return ']';

         when 12 => 
            return ',';

         when 13 => 
            return '.';

         when 14 => 
            return ';';

         when 15 => 
            return '~';

         when 16 => 
            return '!';

         when 17 => 
            return '<';

         when 18 => 
            return '>';

         when 19 => 
            return '=';

         when 20 => 
            return '*';

         when 21 => 
            return '/';

         when 22 => 
            return '-';

         when 23 => 
            return '+';

         when 24 => 
            ENTER(QUOTE);

         when 25 => 
             --  tmpbuf := vstr(yytext(1..YYLength));
                  --  YYLVal := CHARACTER'POS(CHAR(tmpbuf,1));
                  null;
                

         when 26 => 
             ENTER(INITIAL); return T_STRING; 

         when 27 => 
            
                  Error (Line_Number, "missing quote at end of string");
                  ENTER(INITIAL);
                  Line_Number := Line_Number + 1;
                  return T_STRING;
                

         when 28 => 
            ENTER(COMMENT);

         when 29 => 
            null;

         when 30 => 
            null;

         when 31 => 
            Line_Number := Line_Number + 1;

         when 32 => 
            ENTER(INITIAL);

         when 33 => 
              --  yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		   return T_COLOR;
		

         when 34 => 
              --  yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		   return T_NUMBER;
		

         when 35 => 
            
		   --    yylval.low := MAT.Types.Uint64'Value (YYText);
		   return T_NUMBER;


         when 36 => 
             return T_NUMBER; 

         when 37 => 
            return T_UNIT;

         when 38 => 
            return T_UNIT;

         when 39 => 
             return T_UNIT;   

         when 40 => 
            
		   --        yylval.low := MAT.Types.Uint64'Value (YYText);
		   return T_NUMBER;
		

         when 41 => 
            
		   return T_REAL;
		

         when 42 => 
            
		   return T_REAL;
		

         when 43 => 
            
                   --  yylval.low := MAT.Types.Uint64 (MAT.Types.Tick_Value (YYText));
		   return T_REAL;
		

         when 44 => 
            
		  return T_STRING;
		

         when 45 => 
             return T_CALC; 

         when 46 => 
             return T_ATTR; 

         when 47 => 
             return T_TOGGLE; 

         when 48 => 
            
		  return T_CLASS;
		

         when 49 => 
            
		  return T_IDENT;
		

         when 50 => 
            
		  return T_NAME;
		

         when 51 => 
             Error (Line_Number, "illegal character '" & YYText & "'"); 

         when 52 => 
            raise AFLEX_SCANNER_JAMMED;
         when YY_END_OF_BUFFER + INITIAL + 1 |
             YY_END_OF_BUFFER + COMMENT + 1 |
             YY_END_OF_BUFFER + QUOTE + 1 =>
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

end CSS.Parser.Lexer;



