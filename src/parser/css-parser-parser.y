
%token T_NAME
%token T_CLASS
%token T_IDENT
%token T_NUMBER T_REAL T_UNIT
%token T_COLOR
%token T_STRING
%token T_URL
%token T_CALC
%token T_TOGGLE
%token T_ATTR
%token T_UNIT
%token T_IMPORTANT
%token '(' ')'
%token '[' ']'
%token '@'
%token '='
%token '.'
%token '!'
%token '~'
%token '%'
%token '<'
%token '>'

{
   subtype yystype is Integer;
}

%%

css_rules :
     css_rules css_rule
   |
     css_rule
   ;

css_rule :
     css_selectors css_block
   ;

css_selectors :
     css_selectors ',' css_selector
   |
     css_selector
   |
     --  Empty
   ;

css_selector :
     css_selector '>' css_selector
   |
     css_selector '*' css_selector
   |
     css_selector '+' css_selector
   |
     css_selector css_selector
   |
     T_NAME T_CLASS
   |
     T_NAME T_IDENT
   |
     T_NAME
   |
     T_CLASS
   |
     T_IDENT
   ;

css_block :
     '{' css_declaration_list '}'
   |
     '{' error '}'
   ;

css_declaration_list :
     css_declaration_list css_declaration ';'
   |
     css_declaration ';'
          { Error (3, "CSS Declaration"); }
   |
     error ';'
          { Error (4, "Found error rule"); }
   ;

css_declaration :
     ident_token ':' css_value_list important
   |
     ident_token ':' css_value_list
          { Error(2, "CSS rule");
     }

    
   |
     ident_token ':' important
   ;

css_value_list :
     css_value_list css_value
   |
     css_value
   ;

css_value :
     css_calc
   |
     css_toggle
   |
     css_attr
   |
     T_NUMBER
   |
     T_REAL
   |
     T_UNIT
   |
     T_COLOR
   |
     T_NAME
   |
     T_STRING
   |
     T_URL T_STRING ')'
   ;

css_calc :
     T_CALC css_calc_sum ')'
   |
     T_CALC css_calc_sum error
   ;

css_calc_sum :
     css_calc_sum '+' css_calc_product
   |
     css_calc_sum '-' css_calc_product
   |
     css_calc_product
   ;
   
css_calc_product :
     css_calc_product '*' css_calc_value
  |
     css_calc_product '/' css_calc_value
  |
     css_calc_value
  ;

css_calc_value :
     css_calc
  |
     T_NUMBER
  |
     T_UNIT
  ;
     
css_toggle :
     T_TOGGLE css_toggle_values ')'
  ;

css_toggle_values :
     css_toggle_values ',' css_value
  |
     css_value
  ;

css_attr :
     T_ATTR T_NAME T_UNIT ',' css_value_list ')'
 |
     T_ATTR T_NAME T_UNIT ')'
  ;


ident_token:
     T_NAME
   ;

important :
     '!' T_IMPORTANT
   ;

%%
package CSS.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content : in String) return Integer;

end CSS.Parser.Parser;

pragma Style_Checks (Off);
with Interfaces;
with CSS.Parser.Parser_Goto;
with CSS.Parser.Parser_Tokens; 
with CSS.Parser.Parser_Shift_Reduce;
with CSS.Parser.Lexer_IO;
with CSS.Parser.Lexer;
with CSS.Parser.Lexer_Dfa;
with Ada.Text_IO;
package body CSS.Parser.Parser is

   use Ada;
   use CSS.Parser.Lexer;
   use type Ada.Text_IO.Count;
   use type Interfaces.Unsigned_64;

   procedure yyparse;

   procedure yyerror (Message : in String := "syntax error");

   procedure yyerror (Message : in String := "syntax error") is
      pragma Unreferenced (Message);
   begin
      error_count := error_count + 1;
      Error (CSS.Parser.Lexer.Line_Number, Message);
   end yyerror;

   function Parse (Content : in String) return Integer is
   begin
      CSS.Parser.Lexer_IO.Open_Input (Content);
      --  Expr := MAT.Expressions.EMPTY;
      yyparse;
      return 0;
   end Parse;

##%procedure_parse

end CSS.Parser.Parser;
