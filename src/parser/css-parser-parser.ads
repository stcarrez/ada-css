with CSS.Core;
package CSS.Parser.Parser is

   error_count : Natural := 0;

   function Parse (Content  : in String;
                   Document : in CSS.Core.Stylesheet_Access) return Integer;

   --  Set or clear the parser debug flag.
   --  procedure Set_Debug (Flag : in Boolean);

end CSS.Parser.Parser;
