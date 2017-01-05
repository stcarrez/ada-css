pragma Style_Checks (Off);
package Css.Parser.Parser_Tokens is


   subtype yystype is Integer;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, T_Name, T_Class,
         T_Ident, T_Number, T_Real,
         T_Unit, T_Color, T_String,
         T_Url, T_Calc, T_Toggle,
         T_Attr, T_Important, '(',
         ')', '[', ']',
         '@', '=', '.',
         '!', '~', '%',
         '<', '>', ',',
         '*', '+', '{',
         '}', ';', ':',
         '-', '/' );

    Syntax_Error : exception;

end Css.Parser.Parser_Tokens;
