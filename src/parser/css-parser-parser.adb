
pragma Style_Checks (Off);
with Interfaces;
with CSS.Parser.Parser_Goto;
with CSS.Parser.Parser_Tokens; 
with CSS.Parser.Parser_Shift_Reduce;
with CSS.Parser.Lexer_IO;
with CSS.Parser.Lexer;
with CSS.Parser.Lexer_Dfa;
with CSS.Core.Selectors;
with CSS.Core.Styles;
with Ada.Text_IO;
package body CSS.Parser.Parser is

   use Ada;
   use CSS.Parser.Lexer;
   use CSS.Core.Selectors;
   use CSS.Parser.Lexer_Dfa;
   use type Ada.Text_IO.Count;
   use type Interfaces.Unsigned_64;

   procedure yyparse;

   procedure yyerror (Message : in String := "syntax error");

   Document      : CSS.Core.Stylesheet_Access;
   Current_Page  : CSS.Core.Styles.CSSPageRule_Access;
   Current_Rule  : CSS.Core.Styles.CSSStyleRule_Access;

   procedure yyerror (Message : in String := "syntax error") is
      pragma Unreferenced (Message);
   begin
      error_count := error_count + 1;
      Error (CSS.Parser.Lexer_Dfa.yylineno, CSS.Parser.Lexer_Dfa.yylinecol, Message);
   end yyerror;

   function Parse (Content  : in String;
                   Document : in CSS.Core.Stylesheet_Access) return Integer is
   begin
      Error_Count := 0;
      CSS.Parser.Lexer_Dfa.yylineno  := 1;
      CSS.Parser.Lexer_Dfa.yylinecol := 1;
      CSS.Parser.Lexer_IO.Open_Input (Content);
      --  Expr := MAT.Expressions.EMPTY;
      CSS.Parser.Parser.Document := Document;
      yyparse;
      CSS.Parser.Parser.Document := null;
      CSS.Parser.Lexer_IO.Close_Input;
      return Error_Count;

   exception
      when others =>
         CSS.Parser.Parser.Document := null;
         CSS.Parser.Lexer_IO.Close_Input;
         raise;

   end Parse;

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.


procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Css.Parser.Parser_Goto;
    package yy_shift_reduce_tables renames
      Css.Parser.Parser_Shift_Reduce;
    package yy_tokens              renames
      Css.Parser.Parser_Tokens;

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
        DEBUG : constant boolean := TRUE;

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

when 28 => -- #line 143
 Current_Rule := null; 

when 29 => -- #line 148
 Current_Rule := null; Error (
yy.value_stack(yy.tos-2).line, 
yy.value_stack(yy.tos-1).line, "Found @media rule"); 

when 30 => -- #line 154
 Error (
yy.value_stack(yy.tos).line, 
yy.value_stack(yy.tos).column, "Found media_list"); 

when 32 => -- #line 161
 Error (
yy.value_stack(yy.tos-1).line, 
yy.value_stack(yy.tos-1).column, "Found medium (spaces)"); 

when 33 => -- #line 164
 Error (
yy.value_stack(yy.tos).line, 
yy.value_stack(yy.tos).column, "Found medium"); 

when 34 => -- #line 169
 Current_Page := null; 

when 35 => -- #line 172
 Current_Page := null; 

when 36 => -- #line 177
 null; 

when 37 => -- #line 180
 null; 

when 38 => -- #line 185
 Current_Page := new CSS.Core.Styles.CSSPageRule; 

when 42 => -- #line 198
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 43 => -- #line 203
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-3)); 

when 44 => -- #line 206
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 47 => -- #line 217
 Set_Selector_Type (
yyval, SEL_NEXT_SIBLING, yylineno, yylinecol); 

when 48 => -- #line 220
 Set_Selector_Type (
yyval, SEL_CHILD, yylineno, yylinecol); 

when 49 => -- #line 223
 Set_Selector_Type (
yyval, SEL_FOLLOWING_SIBLING, yylineno, yylinecol); 

when 52 => -- #line 234
 Current_Rule := null; 

when 53 => -- #line 237
 Current_Rule := null; 

when 54 => -- #line 240
 Current_Rule := null; Error (
yy.value_stack(yy.tos-5).line, 
yy.value_stack(yy.tos-5).column, "Invalid CSS rule"); 

when 55 => -- #line 245
 Add_Selector_List (Current_Rule, Document, 
yy.value_stack(yy.tos)); 

when 56 => -- #line 248
 Add_Selector_List (Current_Rule, Document, 
yy.value_stack(yy.tos)); 

when 57 => -- #line 251
 Error (
yy.value_stack(yy.tos).line, 
yy.value_stack(yy.tos).column, "Invalid CSS selector component"); 

when 58 => -- #line 256
 Add_Selector (
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-3); 

when 59 => -- #line 259
 Add_Selector (
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-2); 

when 60 => -- #line 262
 
yyval := 
yy.value_stack(yy.tos-1); 

when 61 => -- #line 267
 Add_Selector_Filter (
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 
yyval := 
yy.value_stack(yy.tos-1); 

when 63 => -- #line 274
 Set_Selector (
yyval, SEL_ELEMENT, 
yy.value_stack(yy.tos)); 

when 64 => -- #line 277
 Set_Selector (
yyval, SEL_IDENT, 
yy.value_stack(yy.tos)); 

when 65 => -- #line 280
 Set_Selector (
yyval, SEL_CLASS, 
yy.value_stack(yy.tos)); 

when 68 => -- #line 287
 Set_Selector (
yyval, SEL_NOT, 
yy.value_stack(yy.tos-2)); 

when 73 => -- #line 302
 
yyval := 
yy.value_stack(yy.tos); 

when 74 => -- #line 307
 
yyval := 
yy.value_stack(yy.tos); 

when 81 => -- #line 326
 Set_Selector (
yyval, SEL_HAS_ATTRIBUTE, 
yy.value_stack(yy.tos-2)); 

when 82 => -- #line 329
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 83 => -- #line 332
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 84 => -- #line 335
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).column, "Invalid attribute definition."); 

when 85 => -- #line 340
 Set_Selector_Type (
yyval, SEL_EQ_ATTRIBUTE, yylineno, yylinecol); 

when 86 => -- #line 343
 Set_Selector_Type (
yyval, SEL_CONTAIN_ATTRIBUTE, yylineno, yylinecol); 

when 87 => -- #line 346
 Set_Selector_Type (
yyval, SEL_ORMATCH_ATTRIBUTE, yylineno, yylinecol); 

when 88 => -- #line 349
 Set_Selector_Type (
yyval, SEL_STARTS_ATTRIBUTE, yylineno, yylinecol); 

when 89 => -- #line 352
 Set_Selector_Type (
yyval, SEL_ENDS_ATTRIBUTE, yylineno, yylinecol); 

when 90 => -- #line 355
 Set_Selector_Type (
yyval, SEL_MATCH_ATTRIBUTE, yylineno, yylinecol); 

when 91 => -- #line 361
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 92 => -- #line 364
 Set_Selector (
yyval, SEL_PSEUDO_CLASS, 
yy.value_stack(yy.tos)); 

when 93 => -- #line 367
 Set_Selector (
yyval, SEL_FUNCTION, 
yy.value_stack(yy.tos-3)); 

when 96 => -- #line 378
 
yyval := 
yy.value_stack(yy.tos); 

when 97 => -- #line 383
 
yyval := 
yy.value_stack(yy.tos); 

when 98 => -- #line 386
 
yyval := 
yy.value_stack(yy.tos-4); 

when 99 => -- #line 389
 
yyval := 
yy.value_stack(yy.tos-1); 

when 100 => -- #line 394
 Append_Property (Current_Rule.Style, Document, 
yy.value_stack(yy.tos-2)); 

when 101 => -- #line 397
 Append_Property (Current_Rule.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 102 => -- #line 402
 Set_Property (
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-1), True); 

when 103 => -- #line 405
 Set_Property (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), False); 

when 104 => -- #line 408
 Set_Property (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-2), False); 

when 105 => -- #line 413
 
yyval := 
yy.value_stack(yy.tos-1); 

when 107 => -- #line 422
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 108 => -- #line 425
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 110 => -- #line 434
 
yyval := 
yy.value_stack(yy.tos-1); 

when 111 => -- #line 437
 
yyval := 
yy.value_stack(yy.tos-1); 

when 112 => -- #line 440
 
yyval := 
yy.value_stack(yy.tos-1); 

when 113 => -- #line 443
 
yyval := 
yy.value_stack(yy.tos-1); 

when 114 => -- #line 446
 
yyval := 
yy.value_stack(yy.tos-1); 

when 115 => -- #line 449
 
yyval := 
yy.value_stack(yy.tos-1); 

when 116 => -- #line 452
 
yyval := 
yy.value_stack(yy.tos-1); 

when 117 => -- #line 455
 
yyval := 
yy.value_stack(yy.tos-1); 

when 118 => -- #line 463
 
yyval := 
yy.value_stack(yy.tos); 

when 119 => -- #line 466
 
yyval := 
yy.value_stack(yy.tos); 

when 120 => -- #line 469
 
yyval := 
yy.value_stack(yy.tos-1); 

when 121 => -- #line 472
 
yyval := 
yy.value_stack(yy.tos-1); 

when 122 => -- #line 475
 
yyval := 
yy.value_stack(yy.tos-1); 

when 123 => -- #line 478
 
yyval := 
yy.value_stack(yy.tos); 

when 124 => -- #line 481
 
yyval := 
yy.value_stack(yy.tos); 

when 125 => -- #line 486
 CSS.Parser.Set_Function (
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-2)); 

when 126 => -- #line 489
 Error (
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column, "Invalid function parameter"); 

when 131 => -- #line 506
 
yyval := 
yy.value_stack(yy.tos-1); 

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

   --  Set or clear the parser debug flag.
   -- procedure Set_Debug (Flag : in Boolean) is
   -- begin
   --   yy.DEBUG := Flag;
   -- end Set_Debug;

end CSS.Parser.Parser;
