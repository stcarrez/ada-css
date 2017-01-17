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
%token S

{
   subtype yystype is CSS.Analysis.Parser.YYstype;
}

%%

definitions :
    definitions definition
  |
    definition
  ;

definition :
    property_definition
  |
    named_definition
  ;

spaces :
    spaces S
  |
    S
  |
    --  Empty
  ;

property_definition :
    R_PROPERTY spaces R_DEFINE spaces rule_definition
  ;

named_definition :
    R_DEF_NAME spaces '=' spaces rule_definition
  ;

rule_definition :
    rule_definition R_FOLLOW spaces one_component
  |
    rule_definition R_ANY spaces one_component
  |
    rule_definition '|' spaces one_component
  |
    rule_definition one_component
  |
    one_component
  ;

one_component :
    single_component '+' spaces
  |
    single_component '?' spec_multi
  |
    single_component '*' spec_multi
  |
    single_component '#' spec_multi
  |
    single_component spec_multi
  ;

single_component :
    group_definition
  |
    R_NAME
  |
    R_IDENT
  ;

group_definition :
    '[' spaces rule_definition ']' '!'
  |
    '[' spaces rule_definition ']'
  ;

spec_multi :
    '{' R_NUM ',' R_NUM '}' spaces
  |
    '{' R_NUM ',' '}' spaces
  |
    '{' R_NUM '}' spaces
  |
    spaces
  ;

%%
package CSS.Analysis.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content  : in String) return Integer;

   --  Set or clear the parser debug flag.
   --  procedure Set_Debug (Flag : in Boolean);

end CSS.Analysis.Parser.Parser;

pragma Style_Checks (Off);
with Interfaces;
with CSS.Analysis.Parser.Parser_Goto;
with CSS.Analysis.Parser.Parser_Tokens; 
with CSS.Analysis.Parser.Parser_Shift_Reduce;
with CSS.Analysis.Parser.Lexer_IO;
with CSS.Analysis.Parser.Lexer;
with CSS.Analysis.Parser.Lexer_Dfa;
with Ada.Text_IO;
package body CSS.Analysis.Parser.Parser is

   use Ada;
   use CSS.Analysis.Parser.Lexer;
   use CSS.Analysis.Parser.Lexer_Dfa;
   use type Ada.Text_IO.Count;
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
      --  Expr := MAT.Expressions.EMPTY;
      --  CSS.Parser.Parser.Document := Document;
      yyparse;
      --  CSS.Parser.Parser.Document := null;
      CSS.Analysis.Parser.Lexer_IO.Close_Input;
      --  Parser_Tokens.yylval := EMPTY;
      return Error_Count;

   exception
      when others =>
         --  CSS.Parser.Parser.Document := null;
         CSS.Analysis.Parser.Lexer_IO.Close_Input;
         --  Parser_Tokens.yylval := EMPTY;
         raise;

   end Parse;

##%procedure_parse

   --  Set or clear the parser debug flag.
   -- procedure Set_Debug (Flag : in Boolean) is
   -- begin
   --   yy.DEBUG := Flag;
   -- end Set_Debug;

end CSS.Analysis.Parser.Parser;
