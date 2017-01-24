
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
with CSS.Core.Medias;
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

   Document      : CSS.Core.Sheets.CSSStylesheet_Access;
   Current_Page  : CSS.Core.Styles.CSSPageRule_Access;
   Current_Rule  : CSS.Core.Styles.CSSStyleRule_Access;
   Current_Media : CSS.Core.Medias.CSSMediaRule_Access;

   procedure yyerror (Message : in String := "syntax error") is
      pragma Unreferenced (Message);
   begin
      error_count := error_count + 1;
      Error (CSS.Parser.Lexer_Dfa.yylineno, CSS.Parser.Lexer_Dfa.yylinecol, Message);
   end yyerror;

   function Parse (Content  : in String;
                   Document : in CSS.Core.Sheets.CSSStylesheet_Access) return Integer is
   begin
      Error_Count := 0;
      CSS.Parser.Lexer_Dfa.yylineno  := 1;
      CSS.Parser.Lexer_Dfa.yylinecol := 1;
      CSS.Parser.Lexer_IO.Open_Input (Content);
      CSS.Parser.Parser.Document := Document;
      yyparse;
      CSS.Parser.Parser.Document := null;
      CSS.Parser.Lexer_IO.Close_Input;
      Parser_Tokens.yylval := EMPTY;
      return Error_Count;

   exception
      when others =>
         CSS.Parser.Parser.Document := null;
         CSS.Parser.Lexer_IO.Close_Input;
         Parser_Tokens.yylval := EMPTY;
         raise;

   end Parse;

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.


procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      CSS.Parser.Parser_Goto;
    package yy_shift_reduce_tables renames
      CSS.Parser.Parser_Shift_Reduce;
    package yy_tokens              renames
      CSS.Parser.Parser_Tokens;

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

when 4 => -- #line 78
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Invalid CSS selector component"); 

when 31 => -- #line 156
 Current_Media := null; 

when 32 => -- #line 159
 Current_Media := null; 

when 37 => -- #line 174
 Current_Rule := null; Error (yylval.Line, yylval.Column, "Media condition error"); 

when 38 => -- #line 179
 Current_Rule := null; 

when 39 => -- #line 184
 Current_Rule := null; 

when 40 => -- #line 187
 Error (
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column, "Invalid media selection after " & To_String (
yy.value_stack(yy.tos-2)));  yyerrok; 

when 41 => -- #line 192
 Current_Rule := null; 

when 42 => -- #line 195
 Current_Rule := null; 

when 43 => -- #line 200
 Current_Rule := null; Error (
yy.value_stack(yy.tos-1).line, 
yy.value_stack(yy.tos).line, "Found @<font-face> rule"); 

when 44 => -- #line 205
 Append_Media (Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 45 => -- #line 208
 Append_Media (Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 48 => -- #line 219
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-4).Line, 
yy.value_stack(yy.tos-4).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 49 => -- #line 222
 Set_String (
yyval, "only ", 
yy.value_stack(yy.tos-4).Line, 
yy.value_stack(yy.tos-4).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 50 => -- #line 225
 
yyval := 
yy.value_stack(yy.tos-2); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 51 => -- #line 230
 Set_String (
yyval, " and ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 52 => -- #line 233
 Set_String (
yyval, "", yylval.Line, yylval.Column); 

when 53 => -- #line 238
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 57 => -- #line 249
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 58 => -- #line 252
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 60 => -- #line 259
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 62 => -- #line 266
 Set_String (
yyval, "and ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 63 => -- #line 271
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 65 => -- #line 278
 Set_String (
yyval, "or ", 
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 66 => -- #line 283
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 67 => -- #line 286
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 68 => -- #line 289
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 69 => -- #line 292
 Error (yylval.Line, yylval.Column, "Invalid media in parens");
         Set_String (
yyval, "", yylval.Line, yylval.Column); yyerrok; 

when 70 => -- #line 298
 Set_String (
yyval, "<=", yylval.Line, yylval.Column); 

when 71 => -- #line 301
 Set_String (
yyval, ">=", yylval.Line, yylval.Column); 

when 72 => -- #line 304
 Set_String (
yyval, ">", yylval.Line, yylval.Column); 

when 73 => -- #line 307
 Set_String (
yyval, "<", yylval.Line, yylval.Column); 

when 74 => -- #line 312
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 75 => -- #line 315
 
yyval := 
yy.value_stack(yy.tos-6); Append_String (
yyval, 
yy.value_stack(yy.tos-4)); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 76 => -- #line 318
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 77 => -- #line 321
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, ": "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 79 => -- #line 328
 
yyval := 
yy.value_stack(yy.tos); 

when 80 => -- #line 331
 
yyval := 
yy.value_stack(yy.tos); 

when 81 => -- #line 336
 Current_Page := null; 

when 82 => -- #line 339
 Current_Page := null; 

when 83 => -- #line 344
 null; 

when 84 => -- #line 347
 null; 

when 85 => -- #line 352
 Current_Page := new CSS.Core.Styles.CSSPageRule; 

when 89 => -- #line 365
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 90 => -- #line 370
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 91 => -- #line 373
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 92 => -- #line 378
 
yyval := 
yy.value_stack(yy.tos-1); 

when 93 => -- #line 381
 
yyval := 
yy.value_stack(yy.tos-1); 

when 94 => -- #line 386
 Set_Selector_Type (
yyval, SEL_NEXT_SIBLING, yylineno, yylinecol); 

when 95 => -- #line 389
 Set_Selector_Type (
yyval, SEL_CHILD, yylineno, yylinecol); 

when 96 => -- #line 392
 Set_Selector_Type (
yyval, SEL_FOLLOWING_SIBLING, yylineno, yylinecol); 

when 99 => -- #line 403
 Current_Rule := null; 

when 100 => -- #line 406
 Current_Rule := null; Error (
yy.value_stack(yy.tos-1).line, 
yy.value_stack(yy.tos-1).column, "Invalid CSS rule"); 

when 101 => -- #line 409
 Current_Rule := null; 

when 102 => -- #line 412
 Error (
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column, "Syntax error in CSS rule"); 

when 103 => -- #line 417
 
yyval := 
yy.value_stack(yy.tos-1); 

when 104 => -- #line 420
 
yyval := 
yy.value_stack(yy.tos); 

when 106 => -- #line 427
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Invalid CSS selector component"); 

when 107 => -- #line 432
 Add_Selector_List (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 108 => -- #line 435
 Add_Selector_List (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 109 => -- #line 440
 Add_Selector (
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-3); 

when 110 => -- #line 443
 Add_Selector (
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-2); 

when 111 => -- #line 446
 
yyval := 
yy.value_stack(yy.tos-1); 

when 112 => -- #line 451
 Add_Selector_Filter (
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 
yyval := 
yy.value_stack(yy.tos-1); 

when 114 => -- #line 458
 Set_Selector (
yyval, SEL_ELEMENT, 
yy.value_stack(yy.tos)); 

when 115 => -- #line 461
 Set_Selector (
yyval, SEL_IDENT, 
yy.value_stack(yy.tos)); 

when 116 => -- #line 464
 Set_Selector (
yyval, SEL_CLASS, 
yy.value_stack(yy.tos)); 

when 119 => -- #line 471
 Set_Selector (
yyval, SEL_NOT, 
yy.value_stack(yy.tos-2)); 

when 124 => -- #line 486
 
yyval := 
yy.value_stack(yy.tos); 

when 125 => -- #line 491
 
yyval := 
yy.value_stack(yy.tos); 

when 132 => -- #line 510
 Set_Selector (
yyval, SEL_HAS_ATTRIBUTE, 
yy.value_stack(yy.tos-2)); 

when 133 => -- #line 513
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 134 => -- #line 516
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 135 => -- #line 519
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).column, "Invalid attribute definition."); 

when 136 => -- #line 524
 Set_Selector_Type (
yyval, SEL_EQ_ATTRIBUTE, yylineno, yylinecol); 

when 137 => -- #line 527
 Set_Selector_Type (
yyval, SEL_CONTAIN_ATTRIBUTE, yylineno, yylinecol); 

when 138 => -- #line 530
 Set_Selector_Type (
yyval, SEL_ORMATCH_ATTRIBUTE, yylineno, yylinecol); 

when 139 => -- #line 533
 Set_Selector_Type (
yyval, SEL_STARTS_ATTRIBUTE, yylineno, yylinecol); 

when 140 => -- #line 536
 Set_Selector_Type (
yyval, SEL_ENDS_ATTRIBUTE, yylineno, yylinecol); 

when 141 => -- #line 539
 Set_Selector_Type (
yyval, SEL_MATCH_ATTRIBUTE, yylineno, yylinecol); 

when 142 => -- #line 544
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 143 => -- #line 547
 Set_Selector (
yyval, SEL_PSEUDO_CLASS, 
yy.value_stack(yy.tos)); 

when 144 => -- #line 550
 Set_Selector (
yyval, SEL_FUNCTION, 
yy.value_stack(yy.tos-3)); 

when 147 => -- #line 561
 
yyval := 
yy.value_stack(yy.tos); 

when 148 => -- #line 566
 
yyval := 
yy.value_stack(yy.tos); 

when 149 => -- #line 569
 
yyval := 
yy.value_stack(yy.tos-4); 

when 150 => -- #line 572
 
yyval := 
yy.value_stack(yy.tos-1); 

when 151 => -- #line 577
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos-1)); 

when 152 => -- #line 580
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos));
         Error (
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column, "Invalid property"); yyerrok; 

when 153 => -- #line 584
 
yyval := 
yy.value_stack(yy.tos-2); Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Invalid property (2)"); yyerrok; 

when 154 => -- #line 587
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos-1)); 

when 157 => -- #line 598
 Set_Property (
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-1), True); 

when 158 => -- #line 601
 Set_Property (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), False); 

when 159 => -- #line 604
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Missing ''' or '""' at end of string"); Set_Property (
yyval, 
yy.value_stack(yy.tos-3), EMPTY, False); 

when 160 => -- #line 607
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Invalid property value: " & YYText); Set_Property (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-2), False); 

when 161 => -- #line 610
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Missing ':' after property name"); Set_Property (
yyval, 
yy.value_stack(yy.tos-1), EMPTY, False); 

when 162 => -- #line 613
 Error (yylval.Line, yylval.Column, "Invalid property name"); 
yyval := EMPTY; 

when 163 => -- #line 618
 
yyval := 
yy.value_stack(yy.tos-1); 

when 164 => -- #line 621
 Warning (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "IE7 '*' symbol hack is used"); 
yyval := 
yy.value_stack(yy.tos-1); 

when 166 => -- #line 630
 CSS.Parser.Set_Function (
yyval, Document, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-2)); 

when 167 => -- #line 633
 CSS.Parser.Set_Function (
yyval, Document, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1)); 

when 168 => -- #line 636
 Error (
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column, "Invalid function parameter"); 

when 169 => -- #line 641
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos)); 

when 170 => -- #line 644
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 171 => -- #line 647
 
yyval := 
yy.value_stack(yy.tos); 

when 172 => -- #line 652
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos)); 

when 173 => -- #line 655
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 174 => -- #line 658
 
yyval := 
yy.value_stack(yy.tos); 

when 175 => -- #line 661
 
yyval := 
yy.value_stack(yy.tos-1); --  CSS.Parser.Set_Parameter ($$, Document, $1, $5);
        

when 176 => -- #line 667
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 177 => -- #line 670
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 179 => -- #line 677
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 180 => -- #line 680
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 181 => -- #line 683
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 182 => -- #line 686
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 183 => -- #line 689
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 184 => -- #line 692
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 185 => -- #line 695
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 186 => -- #line 698
 
yyval := 
yy.value_stack(yy.tos); 

when 187 => -- #line 701
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Invalid url()"); 
yyval := EMPTY; 

when 188 => -- #line 706
 
yyval := 
yy.value_stack(yy.tos-1); 

when 189 => -- #line 709
 
yyval := 
yy.value_stack(yy.tos-1); 

when 190 => -- #line 712
 
yyval := 
yy.value_stack(yy.tos-1); 

when 191 => -- #line 715
 
yyval := 
yy.value_stack(yy.tos-1); 

when 192 => -- #line 718
 
yyval := 
yy.value_stack(yy.tos-1); 

when 193 => -- #line 721
 
yyval := 
yy.value_stack(yy.tos-1); 

when 194 => -- #line 724
 
yyval := 
yy.value_stack(yy.tos-1); 

when 195 => -- #line 727
 
yyval := 
yy.value_stack(yy.tos-1); 

when 196 => -- #line 732
 Set_Color (
yyval, 
yy.value_stack(yy.tos-1)); 

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

end CSS.Parser.Parser;
