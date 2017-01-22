
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

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.


procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      CSS.Analysis.Parser.Parser_Goto;
    package yy_shift_reduce_tables renames
      CSS.Analysis.Parser.Parser_Shift_Reduce;
    package yy_tokens              renames
      CSS.Analysis.Parser.Parser_Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       --  Affects error 'Stack size exceeded on state_stack'
       stack_size : constant Natural :=  256;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token:= Error;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
       yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("  -- Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when 9 => -- #line 54
 Create_Property (
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos)); 

when 10 => -- #line 57
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Error in property definition"); 

when 11 => -- #line 62
 Create_Names (
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 
yyval := 
yy.value_stack(yy.tos-2); 

when 12 => -- #line 65
 
yyval := 
yy.value_stack(yy.tos); 
yyval.Names.Append (Ada.Strings.Unbounded.To_String (
yy.value_stack(yy.tos).Token)); 

when 13 => -- #line 70
 Create_Definition (
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos)); 

when 14 => -- #line 73
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Error in named definition"); 

when 16 => -- #line 82
 Append_Group (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), Rules.GROUP_PARAMS); 

when 18 => -- #line 89
 Append_Group (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), Rules.GROUP_DBAR); 

when 20 => -- #line 96
 Append_Group (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), Rules.GROUP_AND); 

when 22 => -- #line 103
 Append_Group (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), Rules.GROUP_ONLY_ONE); 

when 24 => -- #line 110
 Rules.Append (
yy.value_stack(yy.tos-2).Rule.all, 
yy.value_stack(yy.tos-1).Rule); 
yyval := 
yy.value_stack(yy.tos-2); 

when 26 => -- #line 117
 
yy.value_stack(yy.tos-2).Rule.Set_Repeat (1, Natural'Last); 
yyval := 
yy.value_stack(yy.tos-2); 

when 27 => -- #line 120
 
yy.value_stack(yy.tos-2).Rule.Set_Repeat (0, 1); 
yyval := 
yy.value_stack(yy.tos-2); 

when 28 => -- #line 123
 
yy.value_stack(yy.tos-2).Rule.Set_Repeat (0, Natural'Last); 
yyval := 
yy.value_stack(yy.tos-2); 

when 29 => -- #line 126
 
yy.value_stack(yy.tos-2).Rule.Set_Repeat (
yy.value_stack(yy.tos).Min_Repeat, 
yy.value_stack(yy.tos).Max_Repeat, True); 
yyval := 
yy.value_stack(yy.tos-2); 

when 30 => -- #line 129
 
yy.value_stack(yy.tos-1).Rule.Set_Repeat (
yy.value_stack(yy.tos).Min_Repeat, 
yy.value_stack(yy.tos).Max_Repeat); 
yyval := 
yy.value_stack(yy.tos-1); 

when 32 => -- #line 136
 Create_Function (
yyval, 
yy.value_stack(yy.tos-5), 
yy.value_stack(yy.tos-3)); 

when 33 => -- #line 139
 Create_Identifier (
yyval, 
yy.value_stack(yy.tos)); 

when 34 => -- #line 142
 Create_Type_Or_Reference (
yyval, 
yy.value_stack(yy.tos)); 

when 35 => -- #line 145
 Create_Identifier (
yyval, 
yy.value_stack(yy.tos)); 

when 36 => -- #line 150
 
yyval := 
yy.value_stack(yy.tos-2); 

when 37 => -- #line 153
 
yyval := 
yy.value_stack(yy.tos-1); 

when 38 => -- #line 158
 
yyval.Min_Repeat := Get_Value (
yy.value_stack(yy.tos-4)); 
yyval.Max_Repeat := Get_Value (
yy.value_stack(yy.tos-2)); 

when 39 => -- #line 161
 
yyval.Min_Repeat := Get_Value (
yy.value_stack(yy.tos-3)); 
yyval.Max_Repeat := Natural'Last; 

when 40 => -- #line 164
 
yyval.Min_Repeat := Get_Value (
yy.value_stack(yy.tos-2)); 
yyval.Max_Repeat := 
yyval.Min_Repeat; 

when 41 => -- #line 167
 
yyval.Min_Repeat := 1; 
yyval.Max_Repeat := 1; 

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;

end CSS.Analysis.Parser.Parser;
