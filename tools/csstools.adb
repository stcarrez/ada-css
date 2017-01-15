-----------------------------------------------------------------------
--  json -- JSON Reader
--  Copyright (C) 2010, 2011, 2014 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Serialize.IO.JSON;
with Ada.Containers;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with CSS.Parser.Parser;
with CSS.Parser.Lexer;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with CSS.Parser.Lexer_dfa;
with CSS.Core;
with CSS.Core.Refs;
with CSS.Core.Sheets;
with CSS.Core.Vectors;
with CSS.Core.Styles;
with CSS.Core.Selectors;
with CSS.Core.Properties;
with CSS.Core.Errors.Default;
with CSS.Tools.Messages;
procedure CssTools is

   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;

   Count       : constant Natural := Ada.Command_Line.Argument_Count;
   Doc         : aliased CSS.Core.Sheets.CSSStylesheet;
   Err_Handler : aliased CSS.Tools.Messages.Message_List;

   procedure Print (Rule : in CSS.Core.Styles.CSSStyleRule_Access) is
      procedure Print (Prop : in CSS.Core.Properties.CSSProperty) is
      begin
         Ada.Text_IO.Put ("   ");
	 Ada.Text_IO.Put (Prop.Name.all);
	 Ada.Text_IO.Put (": ");
	 Ada.Text_IO.Put (Prop.Value.To_String);
	 Ada.Text_IO.Put_Line (";");
      end Print;
   begin
      Ada.Text_IO.Put_Line (CSS.Core.Selectors.To_String (Rule.Selectors) & " {");
      Rule.Style.Iterate (Print'Access);
      Ada.Text_IO.Put_Line ("}");
   end Print;

   procedure Print_Message (Severity : in CSS.Tools.Messages.Severity_Type;
                            Loc      : in CSS.Core.Location;
                            Message  : in String) is
   begin
      Ada.Text_IO.Put (CSS.Core.To_String (Loc));
      Ada.Text_IO.Put (":");
      Ada.Text_IO.Put_Line (Message);
   end Print_Message;

   procedure Report_Duplicate (Rules : CSS.Core.Vectors.Vector) is
      use type CSS.Core.CSSRule_Type;
      use CSS.Core.Properties;
      use CSS.Core.Styles;

      function Equal (Left, Right : in CSS.Core.Refs.Ref) return Boolean is
         L : CSS.Core.Styles.CSSStyleRule_Access := CSS.Core.Styles.CSSStyleRule'Class (Left.Value.all)'Access;
         R : CSS.Core.Styles.CSSStyleRule_Access := CSS.Core.Styles.CSSStyleRule'Class (Right.Value.all)'Access;
      begin
         return CSSProperty_List (L.Style) = CSSProperty_List (R.Style);
      end Equal;

      procedure Process (Pos : in CSS.Core.Vectors.Cursor) is
         Rule : constant CSSStyleRule_Access := Element (Pos);
         Iter : CSS.Core.Vectors.Cursor;
      begin
         if Rule = null then
            return;
         end if;
         Iter := CSS.Core.Vectors.Next (Pos);
         while CSS.Core.Vectors.Has_Element (Iter) loop
            declare
               Match : constant CSSStyleRule_Access := Element (Iter);
            begin
               if Match /= null then
                  if CSSProperty_List (Rule.Style) = CSSProperty_List (Match.Style) then
                     Ada.Text_IO.Put_Line ("Identical rules:");
                     Ada.Text_IO.Put_Line ("  " & CSS.Core.To_String (Rule.Get_Location)
                                           & ": " & CSS.Core.Selectors.To_String (Rule.Selectors));
                     Ada.Text_IO.Put_Line ("  " & CSS.Core.To_String (Match.Get_Location)
                                           & ": " & CSS.Core.Selectors.To_String (Match.Selectors));
		     Print (Rule);
                  end if;
               end if;
            end;
            CSS.Core.Vectors.Next (Iter);
         end loop;
      end Process;
    begin
       Rules.Iterate (Process'Access);
    end Report_Duplicate;

begin
   CSS.Parser.Lexer_dfa.aflex_debug := False;
   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: csstools file...");
      return;
   end if;

   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);
      begin
         Doc.Set_Href (S);
         CSS.Parser.Load (S, Doc'Unchecked_Access, Err_Handler'Unchecked_Access);
         Report_Duplicate (Doc.Rules);
         Err_Handler.Iterate (Print_Message'Access);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Comments: ");
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (CSS.Parser.Lexer.Current_Comment));
exception
   when E : others =>
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      raise;
end CssTools;
