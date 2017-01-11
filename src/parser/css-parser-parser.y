
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
         { Error ($3.line, $3.column, "Found media_list"); }
  |
     medium
  ;

medium :
     T_IDENT force_spaces
        { Error ($1.line, $1.column, "Found medium (spaces)"); }
  |
     T_IDENT
        { Error ($1.line, $1.column, "Found medium"); }
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
         { Set_Selector_Type ($$, SEL_NEXT_SIBLING, yylineno, yylinecol); }
  |
    '>' spaces
         { Set_Selector_Type ($$, SEL_CHILD, yylineno, yylinecol); }
  |
    '~' spaces
         { Set_Selector_Type ($$, SEL_FOLLOWING_SIBLING, yylineno, yylinecol); }
  ;

unary_operator :
     '-'
  |
     '+'
  ;

ruleset :
    selector_list '{' spaces declaration_list '}' spaces
  |
    selector_list '{' spaces error '}' spaces
       { Error ($1.line, $1.column, "Invalid CSS rule"); }
  ;

selector_list :
    selector_list ',' spaces selector
       { Add_Selector_List ($1, Document, $4); $$ := $1; }
  |
    selector
       { Set_Selector_List ($$, Document, $1); }
  |
    error
       { Error ($1.line, $1.column, "Invalid CSS selector component"); }
  ;

selector :
     selector combinator simple_selector spaces
       { Add_Selector ($1, $3); $$ := $1; }
  |
     selector simple_selector spaces
       { Add_Selector ($1, $2); $$ := $1; }
  |
     simple_selector spaces
       { $$ := $1; }
  ;

simple_selector :
     simple_selector term_selector
       { Add_Selector_Filter ($1, $2); $$ := $1; }
  |
     term_selector
  ;

term_selector :
     type_selector
       { Set_Selector ($$, SEL_ELEMENT, $1); }
  |
     T_HASH
       { Set_Selector ($$, SEL_IDENT, $1); }
  |
     T_CLASS
       { Set_Selector ($$, SEL_CLASS, $1); }
  |
     attrib
  |
     pseudo
  |
     T_NOT negation_arg spaces ')'
       { Set_Selector ($$, SEL_NOT, $2); }
  ;

ns_prefix :
    T_IDENT '|'
  |
    '*' '|'
  |
    '|'
  ;

type_selector :
    ns_prefix element_name
  |
    element_name
       { $$ := $1; }
  ;

element_name :
    T_IDENT
       { $$ := $1; }
  |
    '*'
  ;

negation_arg :
    type_selector
  |
    T_HASH
  |
    T_CLASS
  |
    attrib
  |
    pseudo
  ;

attrib :
     '[' spaces T_IDENT spaces ']'
        { Set_Selector ($$, SEL_HAS_ATTRIBUTE, $3); }
  |
     '[' spaces T_IDENT spaces attrib_op spaces T_IDENT spaces ']'
        { Set_Selector ($$, $5.Sel, $3, $7); }
  |
     '[' spaces T_IDENT spaces attrib_op spaces T_STRING spaces ']'
        { Set_Selector ($$, $5.Sel, $3, $7); }
  |
     '[' error ']'
         { Error ($3.Line, $3.column, "Invalid attribute definition."); }
  ;

attrib_op :
     '='
         { Set_Selector_Type ($$, SEL_EQ_ATTRIBUTE, yylineno, yylinecol); }
  |
     T_INCLUDES
         { Set_Selector_Type ($$, SEL_CONTAIN_ATTRIBUTE, yylineno, yylinecol); }
  |
     T_DASHMATCH
         { Set_Selector_Type ($$, SEL_ORMATCH_ATTRIBUTE, yylineno, yylinecol); }
  |
     T_PREFIXMATCH
         { Set_Selector_Type ($$, SEL_STARTS_ATTRIBUTE, yylineno, yylinecol); }
  |
     T_SUFFIXMATCH
         { Set_Selector_Type ($$, SEL_ENDS_ATTRIBUTE, yylineno, yylinecol); }
  |
     T_SUBSTRINGMATCH
         { Set_Selector_Type ($$, SEL_MATCH_ATTRIBUTE, yylineno, yylinecol); }
  ;

--    ':' [ T_IDENT | T_FUNCTION spaces [T_IDENT spaces ]? ')' ]
pseudo :
    ':' ':' T_IDENT
       { Set_Selector ($$, SEL_PSEUDO_ELEMENT, $3); }
  |
    ':' T_IDENT
       { Set_Selector ($$, SEL_PSEUDO_CLASS, $2); }
  |
    ':' T_FUNCTION spaces pseudo_params ')'
       { Set_Selector ($$, SEL_FUNCTION, $2); }
  ;

pseudo_params :
    pseudo_params T_IDENT spaces
--       { CSS.Parser.Set_Node ($$, TYPE_APPEND, $1, $2); }
  |
    pseudo_params '+' pseudo_value
--       { CSS.Parser.Set_Node ($$, TYPE_ADD, $1, $2); }
  |
    pseudo_value
       { $$ := $1; }
  ;

pseudo_value :
    num_value
       { $$ := $1; }
  |
    T_IDENT spaces '=' spaces num_value
       { $$ := $1; }
  |
    T_IDENT spaces
       { $$ := $1; }
  ;
 
declaration_list :
    declaration_list declaration ';' spaces
       { Append_Property ($1, Document, $2); $$ := $1; }
  |
    declaration_list declaration
       { Append_Property ($1, Document, $2); $$ := $1; }
  |
    declaration ';' spaces
       { Set_Property_List ($$, Document, $1); }
  ;

declaration :
     property ':' spaces expr prio
        { Set_Property ($$, $1, $4, True); }
  |
     property ':' spaces expr
        { Set_Property ($$, $1, $4, False); }
  |
     property ':' error
        { Set_Property ($$, $1, $1, False); }
  ;

property :
     T_IDENT spaces
        { $$ := $1; }
  ;

prio :
     T_IMPORTANT_SYM spaces
  ;

expr :
    expr operator term
       { CSS.Parser.Set_Expr ($$, $1, $2, $3); }
  |
    expr term
       { CSS.Parser.Set_Expr ($$, $1, $2); }
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
     T_FUNCTION spaces function_params ')' spaces
        { CSS.Parser.Set_Function ($$, $1, $3); }
  |
     T_FUNCTION error ')' spaces
        { Error ($1.Line, $1.Column, "Invalid function parameter"); }
  ;

function_params :
     function_params function_param
  |
     function_param
  ;

function_param :
     expr
  |
     T_IDENT spaces '=' spaces num_value spaces
  ;

hexcolor :
     T_HASH spaces
        { $$ := $1; }
  ;

%%
with CSS.Core;
package CSS.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content  : in String;
                   Document : in CSS.Core.Stylesheet_Access) return Integer;

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
with CSS.Core.Selectors;
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

   Document : CSS.Core.Stylesheet_Access;

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

##%procedure_parse

   --  Set or clear the parser debug flag.
   -- procedure Set_Debug (Flag : in Boolean) is
   -- begin
   --   yy.DEBUG := Flag;
   -- end Set_Debug;

end CSS.Parser.Parser;
