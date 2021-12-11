package CSS.Parser.Parser_Tokens is


   subtype YYSType is CSS.Parser.YYstype;

   YYLVal, YYVal : YYSType;
   type Token is
        (END_OF_INPUT, ERROR, T_NAME, T_CLASS,
         T_IDENT, T_ATKEYWORD, T_ATIMPORT,
         T_ATMEDIA, T_NUMBER, T_REAL,
         T_UNIT, T_STRING, T_URL,
         T_URI, T_LENGTH, T_EMS,
         T_EXS, T_TIME, T_FREQ,
         T_CALC, T_TOGGLE, T_ATTR,
         T_IMPORTANT_SYM, T_OR, T_AND,
         T_NOT, T_ONLY, '(',
         ')', '[', ']',
         '@', '=', '.',
         '!', '~', '%',
         '<', '>', T_IMPORT_SYM,
         T_CHARSET_SYM, T_MEDIA_SYM, T_PAGE,
         S, T_CDO, T_CDC,
         T_HASH, T_FUNCTION, T_ANGLE,
         T_INCLUDES, T_PAGE_SYM, T_FONT_FACE_SYM,
         T_PERCENTAGE, T_DASHMATCH, T_DIMENSION,
         T_BAD_STRING, T_BAD_URI, T_PREFIXMATCH,
         T_SUFFIXMATCH, T_SUBSTRINGMATCH, ';',
         '{', '}', ',',
         ':', '/', '+',
         '-', '|', '*');

   Syntax_Error : exception;

end CSS.Parser.Parser_Tokens;
