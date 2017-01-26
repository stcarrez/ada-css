%unit CSS.Analysis.Parser.Parser
%token R_PROPERTY
%token R_DEF_NAME
%token R_IDENT
%token R_NAME
%token R_DEFINE
%token R_FOLLOW
%token R_ANY
%token R_NUM

%token '(' ')'
%token '{' '}'
%token '[' ']'
%token '='
%token '|'
%token '!'
%token '?'
%token '#'
%token '+'
%token '*'
%token ','
%token '/'
%token S

{
   subtype yystype is CSS.Analysis.Parser.YYstype;
}

%%

definitions :
    definitions definition
  |
    definition
  |
    spaces
  |
    error
      { Error (yylval.Line, yylval.Column, "Syntax error"); }
  ;

definition :
    property_definition spaces
  ;

spaces :
    spaces S
  |
    S
  |
    --  Empty
  ;

property_definition :
    property_names spaces R_DEFINE spaces rule_definition
       { Create_Property ($1, $5); }
  |
    property_names spaces '=' spaces rule_definition
       { Create_Property ($1, $5); }
  |
    property_names error
       { Error ($1.Line, $1.Column, "Error in property definition"); }
  ;

property_names :
     property_names spaces R_PROPERTY
        { Create_Names ($1, $3); $$ := $1; }
  |
     property_names spaces R_DEF_NAME
        { Create_Names ($1, $3); $$ := $1; }
  |
     R_PROPERTY
        { $$ := $1; $$.Names.Append (Ada.Strings.Unbounded.To_String ($1.Token)); }
  |
     R_DEF_NAME
        { $$ := $1; $$.Names.Append (Ada.Strings.Unbounded.To_String ($1.Token)); }
  ;

rule_definition :
    rule_comma_definition
  ;

rule_comma_definition :
    rule_comma_definition spaces ',' spaces rule_dbar_definition
       { Append_Group ($$, $1, $5, Rules.GROUP_DBAR); }
  |
    rule_dbar_definition
  ;

rule_dbar_definition :
    rule_dbar_definition spaces R_FOLLOW spaces rule_and_definition
       { Append_Group ($$, $1, $5, Rules.GROUP_DBAR); }
  |
    rule_and_definition
  ;

rule_and_definition :
    rule_and_definition spaces R_ANY spaces rule_or_definition
       { Append_Group ($$, $1, $5, Rules.GROUP_AND); }
  |
    rule_or_definition
  ;

rule_or_definition :
    rule_or_definition spaces '|' spaces rule_cont_definition
       { Append_Group ($$, $1, $5, Rules.GROUP_ONLY_ONE); }
  |
    rule_cont_definition
  ;

rule_cont_definition :
    rule_cont_definition spaces one_component spaces
       { Append_Group ($$, $1, $3, Rules.GROUP_SEQ); }
  |
    rule_cont_definition spaces ',' spaces one_component spaces
       { Append_Group ($$, $1, $5, Rules.GROUP_SEQ); }
  |
    one_component spaces
       { $$ := $1; }
  ;

one_component :
    single_component '+'
       { $1.Rule.Set_Repeat (1, Natural'Last); $$ := $1; }
  |
    single_component '?' spec_multi
       { $1.Rule.Set_Repeat (0, 1); $$ := $1; }
  |
    single_component '*' spec_multi
       { $1.Rule.Set_Repeat (0, Natural'Last); $$ := $1; }
  |
    single_component '#' spec_multi
       { $1.Rule.Set_Repeat ($3.Min_Repeat, $3.Max_Repeat, True); $$ := $1; }
  |
    single_component spec_multi
       { $1.Rule.Set_Repeat ($2.Min_Repeat, $2.Max_Repeat); $$ := $1; }
  ;

single_component :
    group_definition
  |
    term
  ;

term :
    R_IDENT '(' spaces rule_definition spaces ')'
       { Create_Function ($$, $1, $4); }
  |
    R_IDENT
       { Create_Identifier ($$, $1); }
  |
    R_NAME
       { Create_Type_Or_Reference ($$, $1); }
  |
    R_NUM
       { Create_Identifier ($$, $1); }
  |
    ','
       { Create_Identifier ($$, $1); }
  |
    '/'
       { Create_Identifier ($$, $1); }
  ;

param_rule_definition :
    param_rule_definition spaces param_rule_definition
       { Append_Group ($$, $1, $3, Rules.GROUP_PARAMS); }
  |
    param_optional_definition
  ;

param_optional_definition :
    '[' spaces param_rule_definition ']' '?'
       { $$ := $3; }
  |
    '[' spaces param_rule_definition ']'
       { $$ := $3; }
  |
    term
  ;

group_definition :
    '[' spaces rule_definition ']' '!'
       { $$ := $3; }
  |
    '[' spaces rule_definition ']'
       { $$ := $3; }
  ;

spec_multi :
    '{' R_NUM ',' R_NUM '}'
       { $$.Min_Repeat := Get_Value ($2); $$.Max_Repeat := Get_Value ($4); }
  |
    '{' R_NUM ',' '}'
       { $$.Min_Repeat := Get_Value ($2); $$.Max_Repeat := Natural'Last; }
  |
    '{' R_NUM '}'
       { $$.Min_Repeat := Get_Value ($2); $$.Max_Repeat := $$.Min_Repeat; }
  |
    spaces
       { $$.Min_Repeat := 1; $$.Max_Repeat := 1; }
  ;

%%
package CSS.Analysis.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content  : in String) return Integer;

end CSS.Analysis.Parser.Parser;

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

##%procedure_parse

end CSS.Analysis.Parser.Parser;
