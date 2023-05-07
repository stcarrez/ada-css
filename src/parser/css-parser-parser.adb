
with CSS.Parser.Parser_Goto;
with CSS.Parser.Parser_Tokens;
with CSS.Parser.Parser_Shift_Reduce;
with CSS.Parser.Lexer_IO;
with CSS.Parser.Lexer;
with CSS.Parser.Lexer_DFA;
with CSS.Core.Selectors;
with CSS.Core.Styles;
with CSS.Core.Medias;
with Ada.Text_IO;
package body CSS.Parser.Parser is

   use Ada;
   use CSS.Parser.Lexer;
   use CSS.Core.Selectors;
   use CSS.Parser.Lexer_DFA;

   procedure YYParse;

   procedure yyerror (Message : in String := "syntax error");

   Document      : CSS.Core.Sheets.CSSStylesheet_Access;
   Current_Page  : CSS.Core.Styles.CSSPageRule_Access;
   Current_Rule  : CSS.Core.Styles.CSSStyleRule_Access;
   Current_Media : CSS.Core.Medias.CSSMediaRule_Access;

   procedure yyerror (Message : in String := "syntax error") is
   begin
      Error_Count := Error_Count + 1;
      Error (CSS.Parser.Lexer_DFA.yylineno, CSS.Parser.Lexer_DFA.yylinecol, Message);
   end yyerror;

   function Parse (Content  : in String;
                   Document : in CSS.Core.Sheets.CSSStylesheet_Access) return Integer is
   begin
      Error_Count := 0;
      CSS.Parser.Lexer_DFA.yylineno  := 1;
      CSS.Parser.Lexer_DFA.yylinecol := 1;
      CSS.Parser.Lexer_IO.Open_Input (Content);
      CSS.Parser.Parser.Document := Document;
      Current_Rule := null;
      Current_Media := null;
      Current_Page := null;
      YYParse;
      Current_Rule := null;
      Current_Media := null;
      Current_Page := null;
      CSS.Parser.Parser.Document := null;
      CSS.Parser.Lexer_IO.Close_Input;
      Parser_Tokens.YYLVal := EMPTY;
      return Error_Count;

   exception
      when others =>
         CSS.Parser.Parser.Document := null;
         CSS.Parser.Lexer_IO.Close_Input;
         Parser_Tokens.YYLVal := EMPTY;
         raise;

   end Parse;


   procedure YYParse is
      --  Rename User Defined Packages to Internal Names.
      package yy_goto_tables renames
         CSS.Parser.Parser_Goto;
      package yy_shift_reduce_tables renames
         CSS.Parser.Parser_Shift_Reduce;
      package yy_tokens renames
         CSS.Parser.Parser_Tokens;

      use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

      procedure yyerrok;
      procedure yyclearin;
      procedure handle_error;

      subtype goto_row is yy_goto_tables.Row;
      subtype reduce_row is yy_shift_reduce_tables.Row;

      package yy is

         --  the size of the value and state stacks
         --  Affects error 'Stack size exceeded on state_stack'
         stack_size : constant Natural :=  256;

         --  subtype rule         is Natural;
         subtype parse_state is Natural;
         --  subtype nonterminal  is Integer;

         --  encryption constants
         default           : constant := -1;
         first_shift_entry : constant := 0;
         accept_code       : constant := -3001;
         error_code        : constant := -3000;

         --  stack data used by the parser
         tos                : Natural := 0;
         value_stack        : array (0 .. stack_size) of yy_tokens.YYSType;
         state_stack        : array (0 .. stack_size) of parse_state;

         --  current input symbol and action the parser is on
         action             : Integer;
         rule_id            : Rule;
         input_symbol       : yy_tokens.Token := ERROR;

         --  error recovery flag
         error_flag : Natural := 0;
         --  indicates  3 - (number of valid shifts after an error occurs)

         look_ahead : Boolean := True;
         index      : reduce_row;

         --  Is Debugging option on or off
         debug : constant Boolean := False;
      end yy;

      procedure shift_debug (state_id : yy.parse_state; lexeme : yy_tokens.Token);
      procedure reduce_debug (rule_id : Rule; state_id : yy.parse_state);

      function goto_state
         (state : yy.parse_state;
          sym   : Nonterminal) return yy.parse_state;

      function parse_action
         (state : yy.parse_state;
          t     : yy_tokens.Token) return Integer;

      pragma Inline (goto_state, parse_action);

      function goto_state (state : yy.parse_state;
                           sym   : Nonterminal) return yy.parse_state is
         index : goto_row;
      begin
         index := Goto_Offset (state);
         while Goto_Matrix (index).Nonterm /= sym loop
            index := index + 1;
         end loop;
         return Integer (Goto_Matrix (index).Newstate);
      end goto_state;


      function parse_action (state : yy.parse_state;
                             t     : yy_tokens.Token) return Integer is
         index   : reduce_row;
         tok_pos : Integer;
         default : constant Integer := -1;
      begin
         tok_pos := yy_tokens.Token'Pos (t);
         index   := Shift_Reduce_Offset (state);
         while Integer (Shift_Reduce_Matrix (index).T) /= tok_pos
           and then Integer (Shift_Reduce_Matrix (index).T) /= default
         loop
            index := index + 1;
         end loop;
         return Integer (Shift_Reduce_Matrix (index).Act);
      end parse_action;

      --  error recovery stuff

      procedure handle_error is
         temp_action : Integer;
      begin

         if yy.error_flag = 3 then --  no shift yet, clobber input.
            if yy.debug then
               Text_IO.Put_Line ("  -- Ayacc.YYParse: Error Recovery Clobbers "
                                 & yy_tokens.Token'Image (yy.input_symbol));
            end if;
            if yy.input_symbol = yy_tokens.END_OF_INPUT then  -- don't discard,
               if yy.debug then
                  Text_IO.Put_Line ("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
               end if;
               raise yy_tokens.Syntax_Error;
            end if;

            yy.look_ahead := True;   --  get next token
            return;                  --  and try again...
         end if;

         if yy.error_flag = 0 then --  brand new error
            yyerror ("Syntax Error");
         end if;

         yy.error_flag := 3;

         --  find state on stack where error is a valid shift --

         if yy.debug then
            Text_IO.Put_Line ("  -- Ayacc.YYParse: Looking for state with error as valid shift");
         end if;

         loop
            if yy.debug then
               Text_IO.Put_Line ("  -- Ayacc.YYParse: Examining State "
                                 & yy.parse_state'Image (yy.state_stack (yy.tos)));
            end if;
            temp_action := parse_action (yy.state_stack (yy.tos), ERROR);

            if temp_action >= yy.first_shift_entry then
               if yy.tos = yy.stack_size then
                  Text_IO.Put_Line ("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                  raise yy_tokens.Syntax_Error;
               end if;
               yy.tos                  := yy.tos + 1;
               yy.state_stack (yy.tos) := temp_action;
               exit;
            end if;

            if yy.tos /= 0 then
               yy.tos := yy.tos - 1;
            end if;

            if yy.tos = 0 then
               if yy.debug then
                  Text_IO.Put_Line
                     ("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
               end if;
               raise yy_tokens.Syntax_Error;
            end if;
         end loop;

         if yy.debug then
            Text_IO.Put_Line ("  -- Ayacc.YYParse: Shifted error token in state "
                              & yy.parse_state'Image (yy.state_stack (yy.tos)));
         end if;

      end handle_error;

      --  print debugging information for a shift operation
      procedure shift_debug (state_id : yy.parse_state; lexeme : yy_tokens.Token) is
      begin
         Text_IO.Put_Line ("  -- Ayacc.YYParse: Shift "
                           & yy.parse_state'Image (state_id) & " on input symbol "
                           & yy_tokens.Token'Image (lexeme));
      end shift_debug;

      --  print debugging information for a reduce operation
      procedure reduce_debug (rule_id : Rule; state_id : yy.parse_state) is
      begin
         Text_IO.Put_Line ("  -- Ayacc.YYParse: Reduce by rule "
                           & Rule'Image (rule_id) & " goto state "
                           & yy.parse_state'Image (state_id));
      end reduce_debug;

      --  make the parser believe that 3 valid shifts have occured.
      --  used for error recovery.
      procedure yyerrok is
      begin
         yy.error_flag := 0;
      end yyerrok;

      --  called to clear input symbol that caused an error.
      procedure yyclearin is
      begin
         --  yy.input_symbol := YYLex;
         yy.look_ahead := True;
      end yyclearin;

   begin
      --  initialize by pushing state 0 and getting the first input symbol
      yy.state_stack (yy.tos) := 0;

      loop
         yy.index := Shift_Reduce_Offset (yy.state_stack (yy.tos));
         if Integer (Shift_Reduce_Matrix (yy.index).T) = yy.default then
            yy.action := Integer (Shift_Reduce_Matrix (yy.index).Act);
         else
            if yy.look_ahead then
               yy.look_ahead := False;
               yy.input_symbol := YYLex;
            end if;
            yy.action := parse_action (yy.state_stack (yy.tos), yy.input_symbol);
         end if;


         if yy.action >= yy.first_shift_entry then  --  SHIFT

            if yy.debug then
               shift_debug (yy.action, yy.input_symbol);
            end if;

            --  Enter new state
            if yy.tos = yy.stack_size then
               Text_IO.Put_Line (" Stack size exceeded on state_stack");
               raise yy_tokens.Syntax_Error;
            end if;
            yy.tos                  := yy.tos + 1;
            yy.state_stack (yy.tos) := yy.action;
            yy.value_stack (yy.tos) := YYLVal;

            if yy.error_flag > 0 then  --  indicate a valid shift
               yy.error_flag := yy.error_flag - 1;
            end if;

            --  Advance lookahead
            yy.look_ahead := True;

         elsif yy.action = yy.error_code then       -- ERROR
            handle_error;

         elsif yy.action = yy.accept_code then
            if yy.debug then
               Text_IO.Put_Line ("  --  Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

         else --  Reduce Action

            --  Convert action into a rule
            yy.rule_id := Rule (-1 * yy.action);

            --  Execute User Action
            --  user_action(yy.rule_id);
            case yy.rule_id is
               pragma Style_Checks (Off);

when 4 => -- #line 77
 Error (yy.value_stack (yy.tos).Line, yy.value_stack (yy.tos).Column, "Invalid CSS selector component");

when 32 => -- #line 157
 Current_Media := null;

when 33 => -- #line 160
 Current_Media := null;

when 38 => -- #line 175
 Current_Rule := null; Error (yylval.Line, yylval.Column, "Media condition error");

when 39 => -- #line 180
 Current_Rule := null;

when 40 => -- #line 185
 Current_Rule := null;

when 41 => -- #line 188
 Error (yy.value_stack (yy.tos-2).Line, yy.value_stack (yy.tos-2).Column, "Invalid media selection after " & To_String (yy.value_stack (yy.tos-2)));  yyerrok;

when 45 => -- #line 201
 Current_Rule := null;

when 46 => -- #line 206
 Current_Rule := null;

when 47 => -- #line 209
 Current_Rule := null;

when 48 => -- #line 214
 Current_Rule := null; Error (yy.value_stack (yy.tos-1).line, yy.value_stack (yy.tos).line, "Found @<font-face> rule");

when 49 => -- #line 219
 Append_Media (Current_Media, Document, yy.value_stack (yy.tos));

when 50 => -- #line 222
 Append_Media (Current_Media, Document, yy.value_stack (yy.tos));

when 53 => -- #line 233
 Append_String (YYVal, yy.value_stack (yy.tos-1), yy.value_stack (yy.tos));

when 54 => -- #line 238
 Set_String (YYVal, "not ", yy.value_stack (yy.tos-2).Line, yy.value_stack (yy.tos-2).Column); Append_String (YYVal, yy.value_stack (yy.tos));

when 55 => -- #line 241
 Set_String (YYVal, "not ", yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column); Append_String (YYVal, yy.value_stack (yy.tos-1));

when 56 => -- #line 244
 Set_String (YYVal, "only ", yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column); Append_String (YYVal, yy.value_stack (yy.tos-1));

when 57 => -- #line 247
 YYVal := yy.value_stack (yy.tos-2); Append_String (YYVal, yy.value_stack (yy.tos));

when 59 => -- #line 255
 Set_String (YYVal, " and ", yy.value_stack (yy.tos-2).Line, yy.value_stack (yy.tos-2).Column); Append_String (YYVal, yy.value_stack (yy.tos));

when 60 => -- #line 258
 Set_String (YYVal, "", yylval.Line, yylval.Column);

when 63 => -- #line 269
 YYVal := yy.value_stack (yy.tos-1); Append_String (YYVal, " "); Append_String (YYVal, yy.value_stack (yy.tos));

when 65 => -- #line 276
 YYVal := yy.value_stack (yy.tos-1); Append_String (YYVal, " "); Append_String (YYVal, yy.value_stack (yy.tos));

when 67 => -- #line 283
 Set_String (YYVal, "and ", yy.value_stack (yy.tos-2).Line, yy.value_stack (yy.tos-2).Column); Append_String (YYVal, yy.value_stack (yy.tos));

when 68 => -- #line 288
 YYVal := yy.value_stack (yy.tos-1); Append_String (YYVal, " "); Append_String (YYVal, yy.value_stack (yy.tos));

when 70 => -- #line 295
 Set_String (YYVal, "or ", yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column); Append_String (YYVal, yy.value_stack (yy.tos));

when 71 => -- #line 300
 Set_String (YYVal, "(", yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column); Append_String (YYVal, yy.value_stack (yy.tos-3)); Append_String (YYVal, ")");

when 72 => -- #line 303
 Set_String (YYVal, "(", yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column); Append_String (YYVal, yy.value_stack (yy.tos-3)); Append_String (YYVal, ")");

when 73 => -- #line 306
 Set_String (YYVal, "(", yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column); Append_String (YYVal, yy.value_stack (yy.tos-3)); Append_String (YYVal, ")");

when 74 => -- #line 309
 Error (yylval.Line, yylval.Column, "Invalid media in parens");
         Set_String (YYVal, "", yylval.Line, yylval.Column); yyerrok;

when 75 => -- #line 315
 Set_String (YYVal, "<=", yylval.Line, yylval.Column);

when 76 => -- #line 318
 Set_String (YYVal, ">=", yylval.Line, yylval.Column);

when 77 => -- #line 321
 Set_String (YYVal, ">", yylval.Line, yylval.Column);

when 78 => -- #line 324
 Set_String (YYVal, "<", yylval.Line, yylval.Column);

when 79 => -- #line 329
 YYVal := yy.value_stack (yy.tos-4); Append_String (YYVal, yy.value_stack (yy.tos-2)); Append_String (YYVal, yy.value_stack (yy.tos));

when 80 => -- #line 332
 YYVal := yy.value_stack (yy.tos-6); Append_String (YYVal, yy.value_stack (yy.tos-4)); Append_String (YYVal, yy.value_stack (yy.tos-2)); Append_String (YYVal, yy.value_stack (yy.tos));

when 81 => -- #line 335
 YYVal := yy.value_stack (yy.tos-4); Append_String (YYVal, yy.value_stack (yy.tos-2)); Append_String (YYVal, yy.value_stack (yy.tos));

when 82 => -- #line 338
 YYVal := yy.value_stack (yy.tos-4); Append_String (YYVal, ": "); Append_String (YYVal, yy.value_stack (yy.tos));

when 84 => -- #line 345
 YYVal := yy.value_stack (yy.tos);

when 85 => -- #line 348
 YYVal := yy.value_stack (yy.tos);

when 86 => -- #line 353
 Current_Page := null;

when 87 => -- #line 356
 Current_Page := null;

when 88 => -- #line 361
 null;

when 89 => -- #line 364
 null;

when 90 => -- #line 369
 Current_Page := new CSS.Core.Styles.CSSPageRule;

when 94 => -- #line 382
 Set_Selector (YYVal, SEL_PSEUDO_ELEMENT, yy.value_stack (yy.tos));

when 95 => -- #line 387
 Append_Property (Current_Page.Style, Document, yy.value_stack (yy.tos-1));

when 96 => -- #line 390
 Append_Property (Current_Page.Style, Document, yy.value_stack (yy.tos-1));

when 97 => -- #line 395
 YYVal := yy.value_stack (yy.tos-1);

when 98 => -- #line 398
 YYVal := yy.value_stack (yy.tos-1);

when 99 => -- #line 403
 Set_Selector_Type (YYVal, SEL_NEXT_SIBLING, yylineno, yylinecol);

when 100 => -- #line 406
 Set_Selector_Type (YYVal, SEL_CHILD, yylineno, yylinecol);

when 101 => -- #line 409
 Set_Selector_Type (YYVal, SEL_FOLLOWING_SIBLING, yylineno, yylinecol);

when 104 => -- #line 420
 Current_Rule := null;

when 105 => -- #line 423
 Current_Rule := null; Error (yy.value_stack (yy.tos-1).line, yy.value_stack (yy.tos-1).column, "Invalid CSS rule");

when 106 => -- #line 426
 Current_Rule := null;

when 107 => -- #line 429
 Error (yy.value_stack (yy.tos-2).Line, yy.value_stack (yy.tos-2).Column, "Syntax error in CSS rule");

when 108 => -- #line 434
 YYVal := yy.value_stack (yy.tos-1);

when 109 => -- #line 437
 YYVal := yy.value_stack (yy.tos);

when 111 => -- #line 444
 Error (yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column, "Invalid CSS selector component");

when 112 => -- #line 449
 Add_Selector_List (Current_Rule, Current_Media, Document, yy.value_stack (yy.tos));

when 113 => -- #line 452
 Add_Selector_List (Current_Rule, Current_Media, Document, yy.value_stack (yy.tos));

when 114 => -- #line 457
 Add_Selector (yy.value_stack (yy.tos-3), yy.value_stack (yy.tos-2), yy.value_stack (yy.tos-1)); YYVal := yy.value_stack (yy.tos-3);

when 115 => -- #line 460
 Add_Selector (yy.value_stack (yy.tos-2), yy.value_stack (yy.tos-1)); YYVal := yy.value_stack (yy.tos-2);

when 116 => -- #line 463
 YYVal := yy.value_stack (yy.tos-1);

when 117 => -- #line 468
 Add_Selector_Filter (yy.value_stack (yy.tos-1), yy.value_stack (yy.tos)); YYVal := yy.value_stack (yy.tos-1);

when 119 => -- #line 475
 Set_Selector (YYVal, SEL_ELEMENT, yy.value_stack (yy.tos));

when 120 => -- #line 478
 Set_Selector (YYVal, SEL_IDENT, yy.value_stack (yy.tos));

when 121 => -- #line 481
 Set_Selector (YYVal, SEL_CLASS, yy.value_stack (yy.tos));

when 124 => -- #line 488
 Set_Selector (YYVal, SEL_NOT, yy.value_stack (yy.tos-2));

when 129 => -- #line 503
 YYVal := yy.value_stack (yy.tos);

when 130 => -- #line 508
 YYVal := yy.value_stack (yy.tos);

when 137 => -- #line 527
 Set_Selector (YYVal, SEL_HAS_ATTRIBUTE, yy.value_stack (yy.tos-2));

when 138 => -- #line 530
 Set_Selector (YYVal, yy.value_stack (yy.tos-4).Sel, yy.value_stack (yy.tos-6), yy.value_stack (yy.tos-2));

when 139 => -- #line 533
 Set_Selector (YYVal, yy.value_stack (yy.tos-4).Sel, yy.value_stack (yy.tos-6), yy.value_stack (yy.tos-2));

when 140 => -- #line 536
 Error (yy.value_stack (yy.tos).Line, yy.value_stack (yy.tos).column, "Invalid attribute definition.");

when 141 => -- #line 541
 Set_Selector_Type (YYVal, SEL_EQ_ATTRIBUTE, yylineno, yylinecol);

when 142 => -- #line 544
 Set_Selector_Type (YYVal, SEL_CONTAIN_ATTRIBUTE, yylineno, yylinecol);

when 143 => -- #line 547
 Set_Selector_Type (YYVal, SEL_ORMATCH_ATTRIBUTE, yylineno, yylinecol);

when 144 => -- #line 550
 Set_Selector_Type (YYVal, SEL_STARTS_ATTRIBUTE, yylineno, yylinecol);

when 145 => -- #line 553
 Set_Selector_Type (YYVal, SEL_ENDS_ATTRIBUTE, yylineno, yylinecol);

when 146 => -- #line 556
 Set_Selector_Type (YYVal, SEL_MATCH_ATTRIBUTE, yylineno, yylinecol);

when 147 => -- #line 561
 Set_Selector (YYVal, SEL_PSEUDO_ELEMENT, yy.value_stack (yy.tos));

when 148 => -- #line 564
 Set_Selector (YYVal, SEL_PSEUDO_CLASS, yy.value_stack (yy.tos));

when 149 => -- #line 567
 Set_Selector (YYVal, SEL_FUNCTION, yy.value_stack (yy.tos-3));

when 152 => -- #line 578
 YYVal := yy.value_stack (yy.tos);

when 153 => -- #line 583
 YYVal := yy.value_stack (yy.tos);

when 154 => -- #line 586
 YYVal := yy.value_stack (yy.tos-4);

when 155 => -- #line 589
 YYVal := yy.value_stack (yy.tos-1);

when 156 => -- #line 594
 Append_Property (Current_Rule, Current_Media, Document, yy.value_stack (yy.tos-1));

when 157 => -- #line 597
 Append_Property (Current_Rule, Current_Media, Document, yy.value_stack (yy.tos));
         Error (yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column, "Invalid property"); yyerrok;

when 158 => -- #line 601
 YYVal := yy.value_stack (yy.tos-2); Error (yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column, "Invalid property (2)"); yyerrok;

when 159 => -- #line 604
 Append_Property (Current_Rule, Current_Media, Document, yy.value_stack (yy.tos-1));

when 162 => -- #line 615
 Set_Property (YYVal, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos-1), True);

when 163 => -- #line 618
 Set_Property (YYVal, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos), False);

when 164 => -- #line 621
 Error (yy.value_stack (yy.tos).Line, yy.value_stack (yy.tos).Column, "Missing ''' or '""' at end of string");
          Set_Property (YYVal, yy.value_stack (yy.tos-3), EMPTY, False);
          yyclearin;


when 165 => -- #line 627
 Error (yy.value_stack (yy.tos).Line, yy.value_stack (yy.tos).Column, "Invalid property value: " & YYText);
          Set_Property (YYVal, yy.value_stack (yy.tos-2), yy.value_stack (yy.tos-2), False);
          yyclearin;


when 166 => -- #line 633
 Error (yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column, "Missing ':' after property name");
          Set_Property (YYVal, yy.value_stack (yy.tos-1), EMPTY, False);
          yyclearin;


when 167 => -- #line 639
 Error (yylval.Line, yylval.Column, "Invalid property name"); YYVal := EMPTY;

when 168 => -- #line 644
 YYVal := yy.value_stack (yy.tos-1);

when 169 => -- #line 647
 Warning (yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column, "IE7 '*' symbol hack is used"); YYVal := yy.value_stack (yy.tos-1);

when 171 => -- #line 656
 CSS.Parser.Set_Function (YYVal, Document, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos-2));

when 172 => -- #line 659
 CSS.Parser.Set_Function (YYVal, Document, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos-1));

when 173 => -- #line 662
 Error (yy.value_stack (yy.tos-3).Line, yy.value_stack (yy.tos-3).Column, "Invalid function parameter");

when 174 => -- #line 667
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos));

when 175 => -- #line 670
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-1), yy.value_stack (yy.tos));

when 176 => -- #line 673
 YYVal := yy.value_stack (yy.tos);

when 177 => -- #line 678
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos));

when 178 => -- #line 681
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos));

when 179 => -- #line 684
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-1), yy.value_stack (yy.tos));

when 180 => -- #line 687
 YYVal := yy.value_stack (yy.tos);

when 181 => -- #line 690
 YYVal := yy.value_stack (yy.tos-1); --  CSS.Parser.Set_Parameter ($$, Document, $1, $5);


when 182 => -- #line 696
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-2), yy.value_stack (yy.tos));

when 183 => -- #line 699
 CSS.Parser.Set_Expr (YYVal, yy.value_stack (yy.tos-1), yy.value_stack (yy.tos));

when 185 => -- #line 706
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos));

when 186 => -- #line 709
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos));

when 187 => -- #line 712
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos));

when 188 => -- #line 715
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos-1));

when 189 => -- #line 718
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos-1));

when 190 => -- #line 721
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos-1));

when 191 => -- #line 724
 CSS.Parser.Set_Value (YYVal, Document, yy.value_stack (yy.tos));

when 192 => -- #line 727
 YYVal := yy.value_stack (yy.tos);

when 193 => -- #line 730
 Error (yy.value_stack (yy.tos).Line, yy.value_stack (yy.tos).Column, "Invalid url()"); YYVal := EMPTY;

when 194 => -- #line 735
 YYVal := yy.value_stack (yy.tos-1);

when 195 => -- #line 738
 YYVal := yy.value_stack (yy.tos-1);

when 196 => -- #line 741
 YYVal := yy.value_stack (yy.tos-1);

when 197 => -- #line 744
 YYVal := yy.value_stack (yy.tos-1);

when 198 => -- #line 747
 YYVal := yy.value_stack (yy.tos-1);

when 199 => -- #line 750
 YYVal := yy.value_stack (yy.tos-1);

when 200 => -- #line 753
 YYVal := yy.value_stack (yy.tos-1);

when 201 => -- #line 756
 YYVal := yy.value_stack (yy.tos-1);

when 202 => -- #line 761
 Set_Color (YYVal, yy.value_stack (yy.tos-1));
               pragma Style_Checks (On);

               when others => null;
            end case;

            --  Pop RHS states and goto next state
            yy.tos := yy.tos - Rule_Length (yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
               Text_IO.Put_Line (" Stack size exceeded on state_stack");
               raise yy_tokens.Syntax_Error;
            end if;
            yy.state_stack (yy.tos) := goto_state (yy.state_stack (yy.tos - 1),
                                                   Get_LHS_Rule (yy.rule_id));

            yy.value_stack (yy.tos) := YYVal;
            if yy.debug then
               reduce_debug (yy.rule_id,
                  goto_state (yy.state_stack (yy.tos - 1),
                              Get_LHS_Rule (yy.rule_id)));
            end if;

         end if;
      end loop;

   end YYParse;

end CSS.Parser.Parser;
