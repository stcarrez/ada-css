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
with Ada.Unchecked_Deallocation;
package body CSS.Core is

   --  ------------------------------
   --  Get a printable representation of the source file name and line number.
   --  ------------------------------
   function To_String (Loc : in Location) return String is
   begin
      return Util.Log.Locations.To_String (Loc);
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
      return Sheet.Parent;
   end Get_Parent;

   --  ------------------------------
   --  Get the href attribute (stylesheet location).
   --  ------------------------------
   function Get_Href (Sheet : in Stylesheet) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Sheet.Href);
   end Get_Href;

   --  ------------------------------
   --  Get the source file information.
   --  ------------------------------
   function Get_File_Info (Sheet : in Stylesheet) return Util.Log.Locations.File_Info_Access is
   begin
      return Sheet.File;
   end Get_File_Info;

   --  ------------------------------
   --  Set the href attribute representing the stylesheet location.
   --  ------------------------------
   procedure Set_Href (Sheet : in out Stylesheet;
                       Href  : in String) is
   begin
      Sheet.File := Util.Log.Locations.Create_File_Info (Href, Href'First);
      Ada.Strings.Unbounded.Set_Unbounded_String (Sheet.Href, Href);
   end Set_Href;

   --  ------------------------------
   --  Create a location record to represent a CSS source position.
   --  ------------------------------
   function Create_Location (Sheet  : in Stylesheet_Access;
                             Line   : in Natural;
                             Column : in Natural) return Location is
   begin
      return Util.Log.Locations.Create_Line_Info (Sheet.File, Line, Column);
   end Create_Location;

   function Create_Property_Name (Sheet : in Stylesheet;
                                  Name  : in String) return CSSProperty_Name is
      Pos : constant String_Map.Cursor := Sheet.Strings.Find (Name'Unrestricted_Access);
      Res : CSSProperty_Name;
   begin
      if String_Map.Has_Element (Pos) then
         return String_Map.Element (Pos);
      end if;
      Res := new String '(Name);
      Sheet.Strings.Insert (Res.all'Access, Res);
      return Res;
   end Create_Property_Name;

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
      return Rule.Sheet;
   end Get_Stylesheet;

   --  ------------------------------
   --  Get the location of the rule.
   --  ------------------------------
   function Get_Location (Rule : in CSSRule) return Location is
   begin
      return Rule.Loc;
   end Get_Location;

   overriding
   procedure Finalize (Sheet : in out Stylesheet) is
      procedure Free is
        new Ada.Unchecked_Deallocation (String, CSSProperty_Name);
      procedure Free is
         new Ada.Unchecked_Deallocation (String_Map.Map, String_Map_Access);
   begin
      if Sheet.Strings /= null then
         while not Sheet.Strings.Is_Empty loop
            declare
               Pos : String_Map.Cursor := Sheet.Strings.First;
               S   : CSSProperty_Name := String_Map.Element (Pos);
            begin
               Sheet.Strings.Delete (Pos);
               Free (S);
            end;
         end loop;
         Free (Sheet.Strings);
      end if;
   end Finalize;

   --  ------------------------------
   --  Set the source code location.
   --  ------------------------------
   procedure Set_Location (Rule   : in out CSSRule'Class;
                           Line   : in Natural;
                           Column : in Natural;
                           Sheet  : in Stylesheet_Access) is
   begin
      Rule.Loc := Util.Log.Locations.Create_Line_Info (Sheet.File, Line, Column);
   end Set_Location;

end CSS.Core;
