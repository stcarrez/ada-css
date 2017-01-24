pragma Style_Checks (Off);
package CSS.Parser.Parser_Tokens is


   subtype yystype is CSS.Parser.YYstype;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, T_Name, T_Class,
         T_Ident, T_Atkeyword, T_Atimport,
         T_Atmedia, T_Number, T_Real,
         T_Unit, T_String, T_Url,
         T_Uri, T_Length, T_Ems,
         T_Exs, T_Time, T_Freq,
         T_Calc, T_Toggle, T_Attr,
         T_Important_Sym, T_Or, T_And,
         T_Not, T_Only, '(',
         ')', '[', ']',
         '@', '=', '.',
         '!', '~', '%',
         '<', '>', T_Import_Sym,
         T_Charset_Sym, T_Media_Sym, T_Page,
         S, T_Cdo, T_Cdc,
         T_Hash, T_Function, T_Angle,
         T_Includes, T_Page_Sym, T_Font_Face_Sym,
         T_Percentage, T_Dashmatch, T_Dimension,
         T_Bad_String, T_Bad_Uri, T_Prefixmatch,
         T_Suffixmatch, T_Substringmatch, ';',
         '{', '}', ',',
         ':', '/', '+',
         '-', '|', '*' );

    Syntax_Error : exception;

end CSS.Parser.Parser_Tokens;
