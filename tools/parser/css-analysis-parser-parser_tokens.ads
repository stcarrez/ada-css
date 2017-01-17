pragma Style_Checks (Off);
package Css.Analysis.Parser.Parser_Tokens is


   subtype yystype is CSS.Analysis.Parser.YYstype;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, R_Property, R_Def_Name,
         R_Ident, R_Name, R_Define,
         R_Follow, R_Any, R_Num,
         '(', ')', '{',
         '}', '[', ']',
         '=', '|', '!',
         '?', '#', '+',
         '*', ',', S );

    Syntax_Error : exception;

end Css.Analysis.Parser.Parser_Tokens;
