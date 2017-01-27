-----------------------------------------------------------------------
--  css-reports -- CSS Reports
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
with Ada.Strings.Unbounded;
with Util.Log.Locations;
with Util.Strings;
with CSS.Printer;
with CSS.Analysis.Classes;
with CSS.Core.Selectors;
with CSS.Core.Sets;
with CSS.Core.Styles;
with CSS.Core.Refs;
with CSS.Core.Vectors;
with CSS.Tools.Location_Sets;
package body CSS.Reports.Docs is

   procedure Print_Lines (Stream : in out CSS.Printer.File_Type'Class;
                          List   : in CSS.Tools.Location_Sets.Set) is

      Prev_Path : Ada.Strings.Unbounded.Unbounded_String;
      Need_Sep  : Boolean := False;

      procedure Print_One (Pos : in CSS.Tools.Location_Sets.Cursor) is
         Loc  : constant CSS.Core.Location := CSS.Tools.Location_Sets.Element (Pos);
         Path : constant String := Util.Log.Locations.File (Loc);
      begin
         if Path /= Ada.Strings.Unbounded.To_String (Prev_Path) then
            Stream.Print (" ");
            Stream.Print (Path);
            Prev_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
            Stream.Print (": ");
         end if;
         if Need_Sep then
            Stream.Print(" ");
         end if;
         Stream.Print (Util.Strings.Image (Util.Log.Locations.Line (Loc)));
         Need_Sep := True;
      end Print_One;
   begin
      List.Iterate (Print_One'Access);
   end Print_Lines;

   procedure Print (Stream : in out CSS.Printer.File_Type'Class;
                    Data   : in CSS.Analysis.Classes.Map) is
      use CSS.Analysis.Classes;

      procedure Print_Rule (Pos : in CSS.Core.Sets.Cursor) is
         use type CSS.Core.Refs.Ref;
         use type CSS.Core.CSSRule_Access;

         Ref  : CSS.Core.Refs.Ref := CSS.Core.Sets.Element (Pos);
--         Rule : CSS.Core.Styles.CSSStyleRule_Access := CSS.Core.Styles.Element (Pos);
      begin
         if Ref.Value /= null then
           CSS.Printer.Print (Stream, CSS.Core.Styles.CSSStyleRule'Class (Ref.Value.all)'Access);
         end if;
         Stream.New_Line;
      end Print_Rule;

      procedure Print_One (Pos : in CSS.Analysis.Classes.Cursor) is
         Info : Class_Information := CSS.Analysis.Classes.Class_Maps.Element (Pos);
      begin
         Stream.Print (CSS.Analysis.Classes.Class_Maps.Key (Pos));
         Stream.Print ("  ");
         Stream.Print (Natural'Image (Natural (Info.List.Length)));
         Stream.Print ("  Lines: ");
         Print_Lines (Stream, Info.Loc);
         Stream.New_Line;
         Info.List.Iterate (Print_Rule'Access);
         Stream.New_Line;
      end Print_One;

   begin
      Data.Iterate (Print_One'Access);
   end Print;

end CSS.Reports.Docs;
