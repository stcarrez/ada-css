package CSS.Analysis.Parser.Parser_Tokens is


   subtype yystype is CSS.Analysis.Parser.YYstype;

   YYLVal, YYVal : YYSType;
   type Token is
        (END_OF_INPUT, ERROR, R_PROPERTY, R_DEF_NAME,
         R_IDENT, R_NAME, R_DEFINE,
         R_FOLLOW, R_ANY, R_NUM,
         '(', ')', '{',
         '}', '[', ']',
         '=', '|', '!',
         '?', '#', '+',
         '*', ',', '/',
         S);

   Syntax_Error : exception;

end CSS.Analysis.Parser.Parser_Tokens;
