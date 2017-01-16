-----------------------------------------------------------------------
--  css-printer -- CSS printer tools
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with CSS.Core.Selectors;
with CSS.Core.Vectors;
with CSS.Core.Styles;
package body CSS.Printer is

   procedure Do_Indent (Stream : in out Context_Type'Class) is
   begin
      if Stream.Need_Semi then
         Stream.Print (';');
         if not Stream.Compress then
            Stream.New_Line;
         end if;
      end if;
      if not Stream.Compress then
         for I in 1 .. Stream.Indent loop
            Stream.Print (' ');
         end loop;
      end if;
   end Do_Indent;

   procedure Print (Stream : in out Context_Type'Class;
                    Prop   : in CSS.Core.Properties.CSSProperty) is
   begin
      Do_Indent (Stream);
      Stream.Print (Prop.Name.all);
      Stream.Print (':');
      if not Stream.Compress then
         Stream.Print (' ');
      end if;
      Stream.Print (Prop.Value.To_String);
      Stream.Need_Semi := True;
   end Print;

   procedure Print (Stream : in out Context_Type'Class;
                    Rule   : in CSS.Core.Styles.CSSStyleRule_Access) is

      procedure Print (Prop : in CSS.Core.Properties.CSSProperty) is
      begin
         Stream.Print (Prop);
      end Print;

      Sel : constant String := CSS.Core.Selectors.To_String (Rule.Selectors);
   begin
      Stream.Print (Sel);
      if not Stream.Compress then
         Stream.Print (' ');
      end if;
      Stream.Print ('{');
      if not Stream.Compress then
         Stream.New_Line;
      end if;
      Stream.Indent := Stream.Indent + Stream.Indent_Level;
      Stream.Need_Semi := False;
      Rule.Style.Iterate (Print'Access);
      Stream.Need_Semi := False;
      Stream.Indent := Stream.Indent - Stream.Indent_Level;
      if not Stream.Compress then
         if Stream.Full_Semi then
            Stream.Print (';');
         end if;
         Stream.New_Line;
      end if;
      Stream.Print ('}');
      if not Stream.Compress then
         Stream.New_Line;
      end if;
   end Print;

   procedure Print (Stream : in out Context_Type'Class;
                    Sheet  : in CSS.Core.Sheets.CSSStylesheet) is

      procedure Process (Pos : in CSS.Core.Vectors.Cursor) is
         Rule : constant CSS.Core.Styles.CSSStyleRule_Access := CSS.Core.Styles.Element (Pos);
      begin
         Print (Stream, Rule);
      end Process;

   begin
      Sheet.Rules.Iterate (Process'Access);
   end Print;

end CSS.Printer;
