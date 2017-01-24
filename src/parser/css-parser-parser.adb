
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

when 44 => -- #line 200
 Current_Rule := null; 

when 45 => -- #line 205
 Current_Rule := null; 

when 46 => -- #line 208
 Current_Rule := null; 

when 47 => -- #line 213
 Current_Rule := null; Error (
yy.value_stack(yy.tos-1).line, 
yy.value_stack(yy.tos).line, "Found @<font-face> rule"); 

when 48 => -- #line 218
 Append_Media (Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 49 => -- #line 221
 Append_Media (Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 52 => -- #line 232
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-4).Line, 
yy.value_stack(yy.tos-4).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 53 => -- #line 235
 Set_String (
yyval, "only ", 
yy.value_stack(yy.tos-4).Line, 
yy.value_stack(yy.tos-4).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 54 => -- #line 238
 
yyval := 
yy.value_stack(yy.tos-2); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 55 => -- #line 243
 Set_String (
yyval, " and ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 56 => -- #line 246
 Set_String (
yyval, "", yylval.Line, yylval.Column); 

when 57 => -- #line 251
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 61 => -- #line 262
 Set_String (
yyval, "not ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 62 => -- #line 265
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 64 => -- #line 272
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 66 => -- #line 279
 Set_String (
yyval, "and ", 
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 67 => -- #line 284
 
yyval := 
yy.value_stack(yy.tos-1); Append_String (
yyval, " "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 69 => -- #line 291
 Set_String (
yyval, "or ", 
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 70 => -- #line 296
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 71 => -- #line 299
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 72 => -- #line 302
 Set_String (
yyval, "(", 
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column); Append_String (
yyval, 
yy.value_stack(yy.tos-3)); Append_String (
yyval, ")"); 

when 73 => -- #line 305
 Error (yylval.Line, yylval.Column, "Invalid media in parens");
         Set_String (
yyval, "", yylval.Line, yylval.Column); yyerrok; 

when 74 => -- #line 311
 Set_String (
yyval, "<=", yylval.Line, yylval.Column); 

when 75 => -- #line 314
 Set_String (
yyval, ">=", yylval.Line, yylval.Column); 

when 76 => -- #line 317
 Set_String (
yyval, ">", yylval.Line, yylval.Column); 

when 77 => -- #line 320
 Set_String (
yyval, "<", yylval.Line, yylval.Column); 

when 78 => -- #line 325
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 79 => -- #line 328
 
yyval := 
yy.value_stack(yy.tos-6); Append_String (
yyval, 
yy.value_stack(yy.tos-4)); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 80 => -- #line 331
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, 
yy.value_stack(yy.tos-2)); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 81 => -- #line 334
 
yyval := 
yy.value_stack(yy.tos-4); Append_String (
yyval, ": "); Append_String (
yyval, 
yy.value_stack(yy.tos)); 

when 83 => -- #line 341
 
yyval := 
yy.value_stack(yy.tos); 

when 84 => -- #line 344
 
yyval := 
yy.value_stack(yy.tos); 

when 85 => -- #line 349
 Current_Page := null; 

when 86 => -- #line 352
 Current_Page := null; 

when 87 => -- #line 357
 null; 

when 88 => -- #line 360
 null; 

when 89 => -- #line 365
 Current_Page := new CSS.Core.Styles.CSSPageRule; 

when 93 => -- #line 378
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 94 => -- #line 383
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 95 => -- #line 386
 Append_Property (Current_Page.Style, Document, 
yy.value_stack(yy.tos-1)); 

when 96 => -- #line 391
 
yyval := 
yy.value_stack(yy.tos-1); 

when 97 => -- #line 394
 
yyval := 
yy.value_stack(yy.tos-1); 

when 98 => -- #line 399
 Set_Selector_Type (
yyval, SEL_NEXT_SIBLING, yylineno, yylinecol); 

when 99 => -- #line 402
 Set_Selector_Type (
yyval, SEL_CHILD, yylineno, yylinecol); 

when 100 => -- #line 405
 Set_Selector_Type (
yyval, SEL_FOLLOWING_SIBLING, yylineno, yylinecol); 

when 103 => -- #line 416
 Current_Rule := null; 

when 104 => -- #line 419
 Current_Rule := null; Error (
yy.value_stack(yy.tos-1).line, 
yy.value_stack(yy.tos-1).column, "Invalid CSS rule"); 

when 105 => -- #line 422
 Current_Rule := null; 

when 106 => -- #line 425
 Error (
yy.value_stack(yy.tos-2).Line, 
yy.value_stack(yy.tos-2).Column, "Syntax error in CSS rule"); 

when 107 => -- #line 430
 
yyval := 
yy.value_stack(yy.tos-1); 

when 108 => -- #line 433
 
yyval := 
yy.value_stack(yy.tos); 

when 110 => -- #line 440
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Invalid CSS selector component"); 

when 111 => -- #line 445
 Add_Selector_List (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 112 => -- #line 448
 Add_Selector_List (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos)); 

when 113 => -- #line 453
 Add_Selector (
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-3); 

when 114 => -- #line 456
 Add_Selector (
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1)); 
yyval := 
yy.value_stack(yy.tos-2); 

when 115 => -- #line 459
 
yyval := 
yy.value_stack(yy.tos-1); 

when 116 => -- #line 464
 Add_Selector_Filter (
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 
yyval := 
yy.value_stack(yy.tos-1); 

when 118 => -- #line 471
 Set_Selector (
yyval, SEL_ELEMENT, 
yy.value_stack(yy.tos)); 

when 119 => -- #line 474
 Set_Selector (
yyval, SEL_IDENT, 
yy.value_stack(yy.tos)); 

when 120 => -- #line 477
 Set_Selector (
yyval, SEL_CLASS, 
yy.value_stack(yy.tos)); 

when 123 => -- #line 484
 Set_Selector (
yyval, SEL_NOT, 
yy.value_stack(yy.tos-2)); 

when 128 => -- #line 499
 
yyval := 
yy.value_stack(yy.tos); 

when 129 => -- #line 504
 
yyval := 
yy.value_stack(yy.tos); 

when 136 => -- #line 523
 Set_Selector (
yyval, SEL_HAS_ATTRIBUTE, 
yy.value_stack(yy.tos-2)); 

when 137 => -- #line 526
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 138 => -- #line 529
 Set_Selector (
yyval, 
yy.value_stack(yy.tos-4).Sel, 
yy.value_stack(yy.tos-6), 
yy.value_stack(yy.tos-2)); 

when 139 => -- #line 532
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).column, "Invalid attribute definition."); 

when 140 => -- #line 537
 Set_Selector_Type (
yyval, SEL_EQ_ATTRIBUTE, yylineno, yylinecol); 

when 141 => -- #line 540
 Set_Selector_Type (
yyval, SEL_CONTAIN_ATTRIBUTE, yylineno, yylinecol); 

when 142 => -- #line 543
 Set_Selector_Type (
yyval, SEL_ORMATCH_ATTRIBUTE, yylineno, yylinecol); 

when 143 => -- #line 546
 Set_Selector_Type (
yyval, SEL_STARTS_ATTRIBUTE, yylineno, yylinecol); 

when 144 => -- #line 549
 Set_Selector_Type (
yyval, SEL_ENDS_ATTRIBUTE, yylineno, yylinecol); 

when 145 => -- #line 552
 Set_Selector_Type (
yyval, SEL_MATCH_ATTRIBUTE, yylineno, yylinecol); 

when 146 => -- #line 557
 Set_Selector (
yyval, SEL_PSEUDO_ELEMENT, 
yy.value_stack(yy.tos)); 

when 147 => -- #line 560
 Set_Selector (
yyval, SEL_PSEUDO_CLASS, 
yy.value_stack(yy.tos)); 

when 148 => -- #line 563
 Set_Selector (
yyval, SEL_FUNCTION, 
yy.value_stack(yy.tos-3)); 

when 151 => -- #line 574
 
yyval := 
yy.value_stack(yy.tos); 

when 152 => -- #line 579
 
yyval := 
yy.value_stack(yy.tos); 

when 153 => -- #line 582
 
yyval := 
yy.value_stack(yy.tos-4); 

when 154 => -- #line 585
 
yyval := 
yy.value_stack(yy.tos-1); 

when 155 => -- #line 590
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos-1)); 

when 156 => -- #line 593
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos));
         Error (
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column, "Invalid property"); yyerrok; 

when 157 => -- #line 597
 
yyval := 
yy.value_stack(yy.tos-2); Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Invalid property (2)"); yyerrok; 

when 158 => -- #line 600
 Append_Property (Current_Rule, Current_Media, Document, 
yy.value_stack(yy.tos-1)); 

when 161 => -- #line 611
 Set_Property (
yyval, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-1), True); 

when 162 => -- #line 614
 Set_Property (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos), False); 

when 163 => -- #line 617
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Missing ''' or '""' at end of string"); Set_Property (
yyval, 
yy.value_stack(yy.tos-3), EMPTY, False); 

when 164 => -- #line 620
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Invalid property value: " & YYText); Set_Property (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-2), False); 

when 165 => -- #line 623
 Error (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "Missing ':' after property name"); Set_Property (
yyval, 
yy.value_stack(yy.tos-1), EMPTY, False); 

when 166 => -- #line 626
 Error (yylval.Line, yylval.Column, "Invalid property name"); 
yyval := EMPTY; 

when 167 => -- #line 631
 
yyval := 
yy.value_stack(yy.tos-1); 

when 168 => -- #line 634
 Warning (
yy.value_stack(yy.tos-1).Line, 
yy.value_stack(yy.tos-1).Column, "IE7 '*' symbol hack is used"); 
yyval := 
yy.value_stack(yy.tos-1); 

when 170 => -- #line 643
 CSS.Parser.Set_Function (
yyval, Document, 
yy.value_stack(yy.tos-4), 
yy.value_stack(yy.tos-2)); 

when 171 => -- #line 646
 CSS.Parser.Set_Function (
yyval, Document, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos-1)); 

when 172 => -- #line 649
 Error (
yy.value_stack(yy.tos-3).Line, 
yy.value_stack(yy.tos-3).Column, "Invalid function parameter"); 

when 173 => -- #line 654
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos)); 

when 174 => -- #line 657
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 175 => -- #line 660
 
yyval := 
yy.value_stack(yy.tos); 

when 176 => -- #line 665
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos)); 

when 177 => -- #line 668
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-3), 
yy.value_stack(yy.tos)); 

when 178 => -- #line 671
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 179 => -- #line 674
 
yyval := 
yy.value_stack(yy.tos); 

when 180 => -- #line 677
 
yyval := 
yy.value_stack(yy.tos-1); --  CSS.Parser.Set_Parameter ($$, Document, $1, $5);
        

when 181 => -- #line 683
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when 182 => -- #line 686
 CSS.Parser.Set_Expr (
yyval, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when 184 => -- #line 693
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 185 => -- #line 696
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 186 => -- #line 699
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 187 => -- #line 702
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 188 => -- #line 705
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 189 => -- #line 708
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos-1)); 

when 190 => -- #line 711
 CSS.Parser.Set_Value (
yyval, Document, 
yy.value_stack(yy.tos)); 

when 191 => -- #line 714
 
yyval := 
yy.value_stack(yy.tos); 

when 192 => -- #line 717
 Error (
yy.value_stack(yy.tos).Line, 
yy.value_stack(yy.tos).Column, "Invalid url()"); 
yyval := EMPTY; 

when 193 => -- #line 722
 
yyval := 
yy.value_stack(yy.tos-1); 

when 194 => -- #line 725
 
yyval := 
yy.value_stack(yy.tos-1); 

when 195 => -- #line 728
 
yyval := 
yy.value_stack(yy.tos-1); 

when 196 => -- #line 731
 
yyval := 
yy.value_stack(yy.tos-1); 

when 197 => -- #line 734
 
yyval := 
yy.value_stack(yy.tos-1); 

when 198 => -- #line 737
 
yyval := 
yy.value_stack(yy.tos-1); 

when 199 => -- #line 740
 
yyval := 
yy.value_stack(yy.tos-1); 

when 200 => -- #line 743
 
yyval := 
yy.value_stack(yy.tos-1); 

when 201 => -- #line 748
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
