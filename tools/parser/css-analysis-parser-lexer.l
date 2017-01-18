%unit CSS.Analysis.Parser.Lexer
%option case-insensitive
%option yylineno

h               [0-9a-f]
nonascii        [\240-\377]
unicode         \\{h}{1,6}(\r\n|[ \t\r\n\f])?
escape          {unicode}|\\[^\r\n\f0-9a-f]
nmstart         [_a-z]|{nonascii}|{escape}
nmchar          [_a-z0-9-]|{nonascii}|{escape}
num             [0-9]+
comment         ^\#.*$
ident           -?{nmstart}{nmchar}*(\(\))?
name            {nmchar}+
s               [ \t\r\n\f]+
w               {s}?
nl              \n|\r\n|\r|\f

%%

^\#.*$                  null;
\{                      return '{';
\}                      return '}';
\[                      return '[';
\]                      return ']';
\(                      return '(';
\)                      return ')';
\=                      return '=';
\|                      return '|';
\?                      return '?';
\#                      return '#';
\*                      return '*';
\+                      return '+';
\!                      return '!';
\,                      return ',';
{s}		        return S;

":="			{ return R_DEFINE; }
"||"			{ return R_FOLLOW; }
"&&"			{ return R_ANY; }

^{ident}		{ Set_Ident (yylval, YYText, yylineno, yylinecol); return R_PROPERTY; }
^\<{ident}\>		{ Set_Ident (yylval, YYText, yylineno, yylinecol); return R_DEF_NAME; }
{ident}			{ Set_Ident (yylval, YYText, yylineno, yylinecol); return R_IDENT; }
\<{ident}\>		{ Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NAME; }
{num}			{ Set_Ident (yylval, YYText, yylineno, yylinecol); return R_NUM; }
{comment}               { null; }
.                       { Error (yylineno, yylinecol, "Illegal character '" & YYText & "'"); }

%%
with Ada.Strings.Unbounded;
with CSS.Analysis.Parser.Parser_Tokens;
package CSS.Analysis.Parser.Lexer is

   use CSS.Analysis.Parser.Parser_Tokens;

   function YYLex return Token;

end CSS.Analysis.Parser.Lexer;

with Ada.Text_IO;
with CSS.Analysis.Parser.Lexer_dfa;
with CSS.Analysis.Parser.Lexer_IO;
package body CSS.Analysis.Parser.Lexer is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada;
   use CSS.Analysis.Parser.Lexer_dfa;
   use CSS.Analysis.Parser.Lexer_IO;

   pragma Style_Checks (Off);
   pragma Warnings (Off);
##
   pragma Style_Checks (On);

end CSS.Analysis.Parser.Lexer;


