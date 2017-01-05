%unit CSS.Parser.Lexer

%x COMMENT
%x QUOTE

WS      [ \t\f]+
NOT_WS  [^ \t\f\n]
ESCSEQ      \\([^\n]|[0-9]{1,3})

%%

[ \t]+			{ null; }

\n		{ Line_Number := Line_Number + 1; }
\:              return ':';
\@              return '@';
\{              return '{';
\}              return '}';
\(              return '(';
\)              return ')';
\[              return '[';
\]              return ']';
\,              return ',';
\.              return '.';
\;              return ';';
\~              return '~';
\!              return '!';
\<              return '<';
\>              return '>';
\=              return '=';
\*              return '*';
\/              return '/';
\-              return '-';
\+              return '+';

\"              ENTER(QUOTE);
<QUOTE>[^"\n]   { --  tmpbuf := vstr(yytext(1..YYLength));
                  --  YYLVal := CHARACTER'POS(CHAR(tmpbuf,1));
                  null;
                }
<QUOTE>\"       { ENTER(INITIAL); return T_STRING; }
<QUOTE>\n       {
                  Error (Line_Number, "missing quote at end of string");
                  ENTER(INITIAL);
                  Line_Number := Line_Number + 1;
                  return T_STRING;
                }

"/*"                    ENTER(COMMENT);
<COMMENT>[^*\n]*        null;
<COMMENT>\*+[^*/\n]*    null;
<COMMENT>\n             Line_Number := Line_Number + 1;
<COMMENT>"*"+"/"        ENTER(INITIAL);

\#[0-9a-fA-F]+	{  --  yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		   return T_COLOR;
		}
0x[0-9a-fA-F]+	{  --  yylval.low := MAT.Types.Hex_Value (YYText (YYText'First + 2 .. YYText'Last));
		   return T_NUMBER;
		}
\\[0-7]+	{
		   --    yylval.low := MAT.Types.Uint64'Value (YYText);
		   return T_NUMBER;
}
[0-9]+		{
		   --        yylval.low := MAT.Types.Uint64'Value (YYText);
		   return T_NUMBER;
		}
[0-9]+\.[0-9]+	{
                   --  yylval.low := MAT.Types.Uint64 (MAT.Types.Tick_Value (YYText));
		   return T_REAL;
		}
\"		{
		  return T_STRING;
		}
calc\(           { return T_CALC; }
attr\(           { return T_ATTR; }
toggle\(         { return T_TOGGLE; }
\.[A-Za-z_][-A-Za-z0-9_]*	{
		  return T_CLASS;
		}
\#[A-Za-z_][-A-Za-z0-9_]*	{
		  return T_IDENT;
		}
[A-Za-z_][-A-Za-z0-9_]*	{
		  return T_NAME;
		}
.               { Error (Line_Number, "illegal character"); }

%%
with CSS.Parser.Parser_Tokens;
package CSS.Parser.Lexer is

   use CSS.Parser.Parser_Tokens;

   function YYLex return Token;

   Line_Number : Natural := 0;

end CSS.Parser.Lexer;

with CSS.Core;
with Ada.Text_IO;
with CSS.Parser.Lexer_dfa;
with CSS.Parser.Lexer_IO;

package body CSS.Parser.Lexer is

   use Ada.Text_IO;
   use Ada;
   use CSS.Parser.Lexer_dfa;
   use CSS.Parser.Lexer_IO;

   --  Line_Number : Natural := 0;

   pragma Style_Checks (Off);
   pragma Warnings (Off);
##
   pragma Style_Checks (On);

end CSS.Parser.Lexer;


