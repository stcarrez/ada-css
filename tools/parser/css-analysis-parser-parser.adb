
pragma Style_Checks (Off);
with Interfaces;
with Ada.Text_IO;
with CSS.Analysis.Parser.Parser_Goto;
with CSS.Analysis.Parser.Parser_Tokens; 
with CSS.Analysis.Parser.Parser_Shift_Reduce;
with CSS.Analysis.Parser.Lexer_IO;
with CSS.Analysis.Parser.Lexer;
with CSS.Analysis.Parser.Lexer_Dfa;
package body CSS.Analysis.Parser.Parser is

   use Ada;
   use CSS.Analysis.Parser.Lexer;
   use CSS.Analysis.Parser.Lexer_Dfa;
   use type Interfaces.Unsigned_64;

   procedure yyparse;

   procedure yyerror (Message : in String := "syntax error");

   procedure yyerror (Message : in String := "syntax error") is
      pragma Unreferenced (Message);
   begin
      error_count := error_count + 1;
   end yyerror;

   function Parse (Content  : in String) return Integer is
   begin
      Error_Count := 0;
      CSS.Analysis.Parser.Lexer_Dfa.yylineno  := 1;
      CSS.Analysis.Parser.Lexer_Dfa.yylinecol := 1;
      CSS.Analysis.Parser.Lexer_IO.Open_Input (Content);
      yyparse;
      CSS.Analysis.Parser.Lexer_IO.Close_Input;
      return Error_Count;

   exception
      when others =>
         CSS.Analysis.Parser.Lexer_IO.Close_Input;
         raise;

   end Parse;


   procedure YYParse is
      --  Rename User Defined Packages to Internal Names.
      package yy_goto_tables renames
         CSS.Analysis.Parser.Parser_Goto;
      package yy_shift_reduce_tables renames
         CSS.Analysis.Parser.Parser_Shift_Reduce;
      package yy_tokens renames
         CSS.Analysis.Parser.Parser_Tokens;

      use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

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

when 4 => -- #line 38
 Error (yylval.Line, yylval.Column, "Syntax error"); yyclearin;

when 9 => -- #line 55
 Create_Property (yy.value_stack (yy.tos-4), yy.value_stack (yy.tos));

when 10 => -- #line 58
 Create_Property (yy.value_stack (yy.tos-4), yy.value_stack (yy.tos));

when 11 => -- #line 61
 Error (yy.value_stack (yy.tos-1).Line, yy.value_stack (yy.tos-1).Column, "Error in property definition");

when 12 => -- #line 66
 Create_Names (yy.value_stack (yy.tos-2), yy.value_stack (yy.tos)); YYVal := yy.value_stack (yy.tos-2);

when 13 => -- #line 69
 Create_Names (yy.value_stack (yy.tos-2), yy.value_stack (yy.tos)); YYVal := yy.value_stack (yy.tos-2);

when 14 => -- #line 72
 YYVal := yy.value_stack (yy.tos); YYVal.Names.Append (Ada.Strings.Unbounded.To_String (yy.value_stack (yy.tos).Token));

when 15 => -- #line 75
 YYVal := yy.value_stack (yy.tos); YYVal.Names.Append (Ada.Strings.Unbounded.To_String (yy.value_stack (yy.tos).Token));

when 17 => -- #line 84
 Append_Group (YYVal, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos), Rules.GROUP_DBAR);

when 19 => -- #line 91
 Append_Group (YYVal, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos), Rules.GROUP_DBAR);

when 21 => -- #line 98
 Append_Group (YYVal, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos), Rules.GROUP_AND);

when 23 => -- #line 105
 Append_Group (YYVal, yy.value_stack (yy.tos-4), yy.value_stack (yy.tos), Rules.GROUP_ONLY_ONE);

when 25 => -- #line 112
 Append_Group (YYVal, yy.value_stack (yy.tos-3), yy.value_stack (yy.tos-1), Rules.GROUP_SEQ);

when 26 => -- #line 115
 Append_Group (YYVal, yy.value_stack (yy.tos-5), yy.value_stack (yy.tos-1), Rules.GROUP_SEQ);

when 27 => -- #line 118
 YYVal := yy.value_stack (yy.tos-1);

when 28 => -- #line 123
 yy.value_stack (yy.tos-1).Rule.Set_Repeat (1, Natural'Last); YYVal := yy.value_stack (yy.tos-1);

when 29 => -- #line 126
 yy.value_stack (yy.tos-2).Rule.Set_Repeat (0, 1); YYVal := yy.value_stack (yy.tos-2);

when 30 => -- #line 129
 yy.value_stack (yy.tos-2).Rule.Set_Repeat (0, Natural'Last); YYVal := yy.value_stack (yy.tos-2);

when 31 => -- #line 132
 yy.value_stack (yy.tos-2).Rule.Set_Repeat (yy.value_stack (yy.tos).Min_Repeat, yy.value_stack (yy.tos).Max_Repeat, True); YYVal := yy.value_stack (yy.tos-2);

when 32 => -- #line 135
 yy.value_stack (yy.tos-1).Rule.Set_Repeat (yy.value_stack (yy.tos).Min_Repeat, yy.value_stack (yy.tos).Max_Repeat); YYVal := yy.value_stack (yy.tos-1);

when 35 => -- #line 146
 Create_Function (YYVal, yy.value_stack (yy.tos-5), yy.value_stack (yy.tos-2));

when 36 => -- #line 149
 Create_Identifier (YYVal, yy.value_stack (yy.tos));

when 37 => -- #line 152
 Create_Type_Or_Reference (YYVal, yy.value_stack (yy.tos));

when 38 => -- #line 155
 Create_Identifier (YYVal, yy.value_stack (yy.tos));

when 39 => -- #line 158
 Create_Identifier (YYVal, yy.value_stack (yy.tos));

when 40 => -- #line 161
 Create_Identifier (YYVal, yy.value_stack (yy.tos));

when 41 => -- #line 166
 Append_Group (YYVal, yy.value_stack (yy.tos-2), yy.value_stack (yy.tos), Rules.GROUP_PARAMS);

when 43 => -- #line 173
 YYVal := yy.value_stack (yy.tos-2);

when 44 => -- #line 176
 YYVal := yy.value_stack (yy.tos-1);

when 46 => -- #line 183
 YYVal := yy.value_stack (yy.tos-2);

when 47 => -- #line 186
 YYVal := yy.value_stack (yy.tos-1);

when 48 => -- #line 191
 YYVal.Min_Repeat := Get_Value (yy.value_stack (yy.tos-3)); YYVal.Max_Repeat := Get_Value (yy.value_stack (yy.tos-1));

when 49 => -- #line 194
 YYVal.Min_Repeat := Get_Value (yy.value_stack (yy.tos-2)); YYVal.Max_Repeat := Natural'Last;

when 50 => -- #line 197
 YYVal.Min_Repeat := Get_Value (yy.value_stack (yy.tos-1)); YYVal.Max_Repeat := YYVal.Min_Repeat;

when 51 => -- #line 200
 YYVal.Min_Repeat := 1; YYVal.Max_Repeat := 1;
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

end CSS.Analysis.Parser.Parser;
