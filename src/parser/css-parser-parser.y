
%token T_NAME
%token T_CLASS
%token T_IDENT
%token T_ATKEYWORD
%token T_ATIMPORT T_ATMEDIA
%token T_NUMBER T_REAL T_UNIT
%token T_STRING
%token T_URL
%token T_URI
%token T_LENGTH
%token T_EMS
%token T_EXS
%token T_TIME
%token T_FREQ
%token T_CALC
%token T_TOGGLE
%token T_ATTR
%token T_UNIT
%token T_IMPORTANT_SYM
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
%token T_IMPORT_SYM
%token T_CHARSET_SYM
%token T_MEDIA_SYM
%token T_PAGE
%token S
%token T_CDO T_CDC
%token T_HASH
%token T_FUNCTION
%token T_ANGLE
%token T_INCLUDES
%token T_PAGE_SYM
%token T_PERCENTAGE
%token T_DASHMATCH
%token T_DIMENSION
%token T_BAD_STRING
%token T_BAD_URI
%token T_NOT
%token T_PREFIXMATCH
%token T_SUFFIXMATCH
%token T_SUBSTRINGMATCH

{
   subtype yystype is CSS.Parser.YYstype;
}

%%

--    [S|CDO|CDC]* [ import [ CDO S* | CDC S* ]* ]*
--    [ [ ruleset | media | page ] [ CDO S* | CDC S* ]* ]*
--    charset_list spaces_or_comments import_list stylesheet_rules
stylesheet :
    stylesheet_rules
  ;

stylesheet_rules :
    stylesheet_rules stylesheet_rule
  |
    stylesheet_rule
  ;

stylesheet_rule :
    ruleset
  |
    media
  |
    page
  |
    spaces_or_comments
  ;

charset_list :
    charset_list charset
  |
    charset
  |
    --  Empty
  ;

charset :
    T_CHARSET_SYM T_STRING ';' spaces
  ;

import_list :
    import_list spaces_or_comments import
  |
    import
  ;

spaces_or_comments :
    spaces_or_comments space_or_comment
  |
    space_or_comment
  ;

space_or_comment :
    T_CDO
  |
    T_CDC
  |
    spaces
  ;

spaces :
    spaces S
  |
    S
  |
    --  Empty
  ;

force_spaces :
    force_spaces S
  |
    S
  ;

--    [STRING|URI] S* media_list? ';' S*
import :
     T_IMPORT_SYM spaces string_or_uri spaces media_list ';'
  |
     T_IMPORT_SYM spaces string_or_uri spaces ';'
  ;

string_or_uri :
     T_STRING
  |
     T_URI
  ;

-- MEDIA_SYM S* media_list '{' S* ruleset* '}' S*
media :
     T_MEDIA_SYM spaces media_list '{' spaces ruleset '}' spaces
  ;

--  medium [ COMMA S* medium]*
media_list :
     media_list ',' medium
         { Error (20, "Found media_list"); }
  |
     medium
  ;

medium :
     T_IDENT force_spaces
        { Error (10, "Found medium (spaces)"); }
  |
     T_IDENT
        { Error (10, "Found medium"); }
  ;

--    '{' spaces declaration? [ ';' spaces declaration? ]* '}' spaces
page
  : T_PAGE_SYM spaces pseudo_page
    '{' spaces declaration_list '}' spaces
  ;

declaration_list :
    declaration_list ';' spaces declaration
  |
    declaration
  ;

pseudo_page
  : ':' T_IDENT spaces
  |
    --  Empty
  ;
operator :
    '/' spaces
  |
    ',' spaces
  ;

combinator :
    '+' spaces
  |
    '>' spaces
  |
    '~' spaces
  ;

unary_operator :
     '-'
  |
     '+'
  ;

--    ruleset ruleset
--  |
ruleset :
    selector_list '{' spaces declaration_list '}' spaces
  |
    selector_list '{' spaces error '}' spaces
       { Error (12, "Invalid CSS rule"); }
  ;

selector_list :
    selector_list ',' spaces selector
  |
    selector
  |
    error
       { Error (13, "Invalid CSS selector component"); }
  ;

--  : simple_selector [ combinator selector | spaces [ combinator? selector ]? ]?
selector :
     selector combinator simple_selector_seq
  |
     selector simple_selector_seq
  |
     simple_selector_seq
  ;

sel :
--    combinator selector spaces combinator selector
--  |
    combinator selector
  ;

simple_selector_seq :
    type_selector
    element_name simple_selector_list
  |
    simple_selector_list
  |
    element_name spaces
  ;

simple_selector_list :
     simple_selector_list term_selector
 |
     term_selector
  ;

term_selector :
     T_HASH spaces
  |
     T_CLASS spaces
  |
     attrib spaces
  |
     pseudo spaces
  |
     T_NOT negation_arg spaces ')' spaces
  ;

negation_arg :
    type_selector
  |
    universal
  |
    T_HASH
  |
    T_CLASS
  |
    attrib
  |
    pseudo
  ;

universal :
    '*'
  ;

type_selector :
    element_name
  ;

element_name :
    T_IDENT
        { Error (23, "Found element name"); }
  |
    '*'
  ;

--   '[' spaces IDENT spaces [ [ '=' | INCLUDES | DASHMATCH ] spaces
--    [ IDENT | STRING ] spaces ]? ']'
attrib :
     '[' spaces T_IDENT spaces attrib_sel ']'
  ;

attrib_sel :
     attrib_sel attrib_sel
  |
     attrib_op spaces T_IDENT spaces
  |
     attrib_op spaces T_STRING spaces
  ;

attrib_op :
     '='
  |
     T_INCLUDES
  |
     T_DASHMATCH
  |
     T_PREFIXMATCH
  |
     T_SUFFIXMATCH
  |
     T_SUBSTRINGMATCH
  |
     --  Empty
  ;

--    ':' [ T_IDENT | T_FUNCTION spaces [T_IDENT spaces ]? ')' ]
pseudo :
    ':' ':' T_IDENT
  |
    ':' T_IDENT
  |
    ':' T_FUNCTION spaces pseudo_params ')'
  ;

pseudo_params :
    pseudo_params T_IDENT spaces
  |
    pseudo_params '+' pseudo_value
  |
    pseudo_value
  ;

pseudo_value :
    num_value
  |
    T_IDENT spaces
  ;
 
declaration_list :
    declaration_list declaration ';' spaces
  |
    declaration_list declaration
       { Error (100, "Rule without ';'"); }
  |
    declaration ';' spaces
  ;

declaration :
     property ':' spaces expr prio
  |
     property ':' spaces expr
  |
     property ':' error
  ;

property :
      T_IDENT spaces
          { Error (4, "Property found"); }
  ;

prio :
     T_IMPORTANT_SYM spaces
  ;

expr :
    expr operator term
  |
    expr term
  |
    term
  ;

--    [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
--      TIME S* | FREQ S* ]
num_value :
      T_NUMBER spaces
        { $$ := $1; }
  |
      T_PERCENTAGE spaces
        { $$ := $1; }
  |
      T_LENGTH spaces
        { $$ := $1; }
  |
      T_EMS spaces
        { $$ := $1; }
  |
      T_EXS spaces
        { $$ := $1; }
  |
      T_ANGLE spaces
        { $$ := $1; }
  |
      T_TIME spaces
        { $$ := $1; }
  |
      T_FREQ spaces
        { $$ := $1; }
  ;

--  : unary_operator? term_value
--    [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
--      TIME S* | FREQ S* ]
term :
     unary_operator num_value
        { $$ := $2; }
  |
     num_value
        { $$ := $1; }
  |
     T_STRING spaces
        { $$ := $1; }
  |
     T_IDENT spaces
        { $$ := $1; }
  |
     T_URI spaces
        { $$ := $1; }
  |
     hexcolor
        { $$ := $1; }
  |
     function
        { $$ := $1; }
  ;

function :
     T_FUNCTION spaces expr ')' spaces
  ;

hexcolor :
     T_HASH spaces
        { $$ := $1; }
  ;

%%
package CSS.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content : in String) return Integer;

   --  Set or clear the parser debug flag.
   --  procedure Set_Debug (Flag : in Boolean);

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
      Error (CSS.Parser.Lexer_Dfa.yylineno, Message);
   end yyerror;

   function Parse (Content : in String) return Integer is
   begin
      CSS.Parser.Lexer_IO.Open_Input (Content);
      --  Expr := MAT.Expressions.EMPTY;
      yyparse;
      return 0;
   end Parse;

##%procedure_parse

   --  Set or clear the parser debug flag.
   -- procedure Set_Debug (Flag : in Boolean) is
   -- begin
   --   yy.DEBUG := Flag;
   -- end Set_Debug;

end CSS.Parser.Parser;
