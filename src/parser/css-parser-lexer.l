%unit CSS.Parser.Lexer
%option case-insensitive
%option yylineno

%x COMMENT
%x QUOTE

h               [0-9a-f]
nonascii        [\240-\377]
unicode         \\{h}{1,6}(\r\n|[ \t\r\n\f])?
escape          {unicode}|\\[^\r\n\f0-9a-f]
nmstart         [_a-z]|{nonascii}|{escape}
nmchar          [_a-z0-9-]|{nonascii}|{escape}
string1         \"([^\n\r\f\\"]|\\{nl}|{escape})*\"
string2         \'([^\n\r\f\\']|\\{nl}|{escape})*\'
badstring1      \"([^\n\r\f\\"]|\\{nl}|{escape})*\\?
badstring2      \'([^\n\r\f\\']|\\{nl}|{escape})*\\?
baduri1         url\({w}([!#$%&*-\[\]-~]|{nonascii}|{escape})*{w}
baduri2         url\({w}{string}{w}
baduri3         url\({w}{badstring}
comment         \/\*[^*]*\*+([^/*][^*]*\*+)*\/
ident           -?{nmstart}{nmchar}*
name            {nmchar}+
num             [0-9]+|[0-9]*"."[0-9]+
string          {string1}|{string2}
badstring       {badstring1}|{badstring2}
baduri          {baduri1}|{baduri2}|{baduri3}
url             ([!#$%&*-~]|{nonascii}|{escape})*
s               [ \t\r\n\f]+
w               {s}?
nl              \n|\r\n|\r|\f

A               a|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?
C               c|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?
D               d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
E               e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
G               g|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\g
H               h|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\h
I               i|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\i
K               k|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\k
L               l|\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?|\\l
M               m|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\m
N               n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
O               o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
P               p|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\p
R               r|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\r
S               s|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\s
T               t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
U               u|\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\u
X               x|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\x
Z               z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\z

%%
\/\*                    Append (Current_Comment, "/*"); ENTER(COMMENT);
<COMMENT>[^*\n]*        Append (Current_Comment, YYText);
<COMMENT>\*+[^*/\n]*    Append (Current_Comment, YYText);
<COMMENT>\n             Append (Current_Comment, YYText);
<COMMENT>"*"+"/"        Append (Current_Comment, YYText); ENTER(INITIAL);
<COMMENT><<EOF>>        Text_IO.Put_Line ("End of comment error!"); ENTER(INITIAL);

\:                      return ':';
\-                      return '-';
\+                      return '+';
\;                      return ';';
\,                      return ',';
\{                      return '{';
\}                      return '}';
\[                      return '[';
\]                      return ']';
\(                      return '(';
\)                      return ')';
\~                      return '~';
\<                      return '<';
\>                      return '>';
\=                      return '=';
\*                      return '*';
\/                      return '/';
\.                      return '.';
{s}		        return S;

"<!--"		        { return T_CDO; }
"-->"			{ return T_CDC; }
"~="			{ return T_INCLUDES; }
"|="			{ return T_DASHMATCH; }
"$="			{ return T_SUFFIXMATCH; }
"*="			{ return T_SUBSTRINGMATCH; }
"^="			{ return T_PREFIXMATCH; }

{string}		{ CSS.Parser.Set_Value (yylval, YYText, V_STRING); return T_STRING; }
{badstring}             { CSS.Parser.Set_Value (yylval, YYText, V_STRING); return T_BAD_STRING; }
{ident}			{ CSS.Parser.Set_Value (yylval, YYText, V_IDENT); return T_IDENT; }

@{I}{M}{P}{O}{R}{T}	{ return T_IMPORT_SYM; }
@{P}{A}{G}{E}		{ return T_PAGE_SYM; }
@{M}{E}{D}{I}{A}	{ return T_MEDIA_SYM; }
"@charset "		{ return T_CHARSET_SYM; }
"@"{name}		{ CSS.Parser.Set_Value (yyval, YYText, V_IDENT); return T_ATKEYWORD; }

"!"({w}|{comment})*{I}{M}{P}{O}{R}{T}{A}{N}{T}	{return T_IMPORTANT_SYM; }

{num}{E}{M}		{ CSS.Parser.Set_Value (yylval, YYText, U_EM); return T_EMS; }
{num}{E}{X}		{ CSS.Parser.Set_Value (yylval, YYText, U_EX); return T_EXS; }
{num}{P}{X}		{ CSS.Parser.Set_Value (yylval, YYText, U_PX); return T_LENGTH; }
{num}{C}{M}		{ CSS.Parser.Set_Value (yylval, YYText, U_CM); return T_LENGTH; }
{num}{M}{M}		{ CSS.Parser.Set_Value (yylval, YYText, U_MM); return T_LENGTH; }
{num}{I}{N}		{ CSS.Parser.Set_Value (yylval, YYText, U_IN); return T_LENGTH; }
{num}{P}{T}		{ CSS.Parser.Set_Value (yylval, YYText, U_PT); return T_LENGTH; }
{num}{P}{C}		{ CSS.Parser.Set_Value (yylval, YYText, U_PC); return T_LENGTH; }
{num}{D}{E}{G}		{ CSS.Parser.Set_Value (yylval, YYText, U_DEG); return T_ANGLE; }
{num}{R}{A}{D}		{ CSS.Parser.Set_Value (yylval, YYText, U_RAD); return T_ANGLE; }
{num}{G}{R}{A}{D}	{ CSS.Parser.Set_Value (yylval, YYText, U_GRAD); return T_ANGLE; }
{num}{M}{S}		{ CSS.Parser.Set_Value (yylval, YYText, U_MS); return T_TIME; }
{num}{S}		{ CSS.Parser.Set_Value (yylval, YYText, U_SEC); return T_TIME; }
{num}{H}{Z}		{ CSS.Parser.Set_Value (yylval, YYText, U_HZ); return T_FREQ; }
{num}{K}{H}{Z}		{ CSS.Parser.Set_Value (yylval, YYText, U_KHZ); return T_FREQ; }
{num}{ident}		{ CSS.Parser.Set_Value (yylval, YYText, U_NONE); return T_DIMENSION; }

{num}%			{ CSS.Parser.Set_Value (yylval, YYText, U_NONE); return T_PERCENTAGE; }
{num}			{ CSS.Parser.Set_Value (yylval, YYText, U_NONE); return T_NUMBER; }

"url("{w}{string}{w}")" { CSS.Parser.Set_Value (yylval, YYText, V_URL); return T_URI; }
"url("{w}{url}{w}")"    { CSS.Parser.Set_Value (yylval, YYText, V_URL); return T_URI; }
{baduri}                { CSS.Parser.Set_Value (yylval, YYText, V_URL); return T_BAD_URI; }

\:not"("                { return T_NOT; }
{ident}"("		{ CSS.Parser.Set_Value (yylval, YYText, V_FUNCTION); return T_FUNCTION; }

"#"{name}		{ CSS.Parser.Set_Value (yylval, YYText, V_IDENT); return T_HASH; }
\.{name}	        { CSS.Parser.Set_Value (yylval, YYText, V_IDENT); return T_CLASS; }

\!              return '!';

\"              ENTER(QUOTE);
<QUOTE>[^"\n]   { --  tmpbuf := vstr(yytext(1..YYLength));
                  --  YYLVal := CHARACTER'POS(CHAR(tmpbuf,1));
                  null;
                }
<QUOTE>\"       { ENTER(INITIAL); return T_STRING; }
<QUOTE>\n       {
                  ENTER(INITIAL);
                  return T_STRING;
                }
.               { Error (yylineno, "illegal character '" & YYText & "'"); }

%%
with Ada.Strings.Unbounded;
with CSS.Parser.Parser_Tokens;
package CSS.Parser.Lexer is

   use CSS.Parser.Parser_Tokens;

   function YYLex return Token;

   Current_Comment : Ada.Strings.Unbounded.Unbounded_String;

end CSS.Parser.Lexer;

with CSS.Core;
with Ada.Text_IO;
with CSS.Parser.Lexer_dfa;
with CSS.Parser.Lexer_IO;

package body CSS.Parser.Lexer is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada;
   use CSS.Parser.Lexer_dfa;
   use CSS.Parser.Lexer_IO;

   pragma Style_Checks (Off);
   pragma Warnings (Off);
##
   pragma Style_Checks (On);

end CSS.Parser.Lexer;


