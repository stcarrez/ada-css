-----------------------------------------------------------------------
--  css-core -- Core CSS API definition
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

package body CSS.Core is

   --  ------------------------------
   --  Get the line number.
   --  ------------------------------
   function Get_Line (Loc : in Location) return Natural is
   begin
      return Loc.Line;
   end Get_Line;

   --  ------------------------------
   --  Get the source file or URI.
   --  ------------------------------
   function Get_Source (Loc : in Location) return String is
   begin
      if Loc.Sheet = null then
         return "";
      else
         return Loc.Sheet.Get_Href;
      end if;
   end Get_Source;

   --  ------------------------------
   --  Get a printable representation of the source file name and line number.
   --  ------------------------------
   function To_String (Loc : in Location) return String is
      Line : constant String := Natural'Image (Loc.Line);
   begin
      return Get_Source (Loc) & ":" & Line (Line'First + 1 .. Line'Last);
   end To_String;

   --  ------------------------------
   --  Returns the CSS type ("text/css").
   --  ------------------------------
   function Get_Type (Sheet : in Stylesheet) return String is
      pragma Unreferenced (Sheet);
   begin
      return "text/css";
   end Get_Type;

   --  ------------------------------
   --  Get the parent CSS stylesheet if there is one or return null.
   --  ------------------------------
   function Get_Parent (Sheet : in Stylesheet) return Stylesheet_Access is
   begin
      return Sheet.Loc.Sheet;
   end Get_Parent;

   --  ------------------------------
   --  Get the href attribute (stylesheet location).
   --  ------------------------------
   function Get_Href (Sheet : in Stylesheet) return String is
   begin
      return "";
   end Get_Href;

   --  ------------------------------
   --  Get the parent rule.  Returns null when there is no parent.
   --  ------------------------------
   function Get_Parent (Rule : in CSSRule) return CSSRule_Access is
   begin
      return Rule.Parent;
   end Get_Parent;

   --  ------------------------------
   --  Get the stylesheet.
   --  ------------------------------
   function Get_Stylesheet (Rule : in CSSRule) return Stylesheet_Access is
   begin
      return Rule.Loc.Sheet;
   end Get_Stylesheet;

   --  ------------------------------
   --  Get the location of the rule.
   --  ------------------------------
   function Get_Location (Rule : in CSSRule) return Location is
   begin
      return Rule.Loc;
   end Get_Location;

end CSS.Core;
