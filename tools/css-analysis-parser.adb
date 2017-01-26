-----------------------------------------------------------------------
--  css-analysis-parser -- Rule parser for CSS Analysis
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
with Ada.Directories;
with Util.Strings;
with Util.Log.Locations;
with CSS.Analysis.Parser.Parser;
with CSS.Analysis.Parser.Parser_Tokens;
package body CSS.Analysis.Parser is

   use Ada.Strings.Unbounded;

   Current_File       : Util.Log.Locations.File_Info_Access;
   Current_Repository : access CSS.Analysis.Rules.Repository_Type;

   --  ------------------------------
   --  Load all the rule definition files stored in the given directory.
   --  ------------------------------
   procedure Load_All (Path       : in String;
                       Repository : access Rules.Repository_Type) is
      use Ada.Directories;

      Search      : Search_Type;
      Filter      : constant Filter_Type := (Ordinary_File => True, others => False);
      Ent         : Directory_Entry_Type;
   begin
      if not Exists (Path) then
         Log.Info ("Path {0} does not exist.", Path);
         return;
      end if;

      if Kind (Path) /= Directory then
         Log.Error ("Path '{0}' is not a directory.", Path);
         return;
      end if;

      Log.Info ("Scanning directory '{0}' for rule definitions", Path);
      Start_Search (Search, Directory => Path, Pattern => "*.def", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Full_Path : constant String := Full_Name (Ent);
         begin
            Load (Full_Path, Repository);
         end;
      end loop;
   end Load_All;

   --  ------------------------------
   --  Load the rule definition file and populate the rule repository.
   --  ------------------------------
   procedure Load (Path       : in String;
                   Repository : access Rules.Repository_Type) is
      Res : Integer;
   begin
      Log.Info ("Loading rule definition file {0}", Path);
      Current_File := Util.Log.Locations.Create_File_Info (Path, Path'First);
      Current_Repository := Repository;
      Res := CSS.Analysis.Parser.Parser.Parse (Path);
      if Res /= 0 then
         Log.Error ("Found {0} errors while parsing {1}", Util.Strings.Image (Res), Path);
      end if;

   exception
      when Parser_Tokens.Syntax_Error =>
         Log.Error ("Syntax error while parsing {0}", Path);

   end Load;

   --  ------------------------------
   --  Set the token to represent an identifier at the given source position.
   --  ------------------------------
   procedure Set_Ident (Into   : in out YYstype;
                        Text   : in String;
                        Line   : in Natural;
                        Column : in Natural) is
   begin
      Log.Debug ("Identifier {0}", Text);

      Into.Rule   := null;
      Into.Token  := To_Unbounded_String (Text);
      Into.Line   := Line;
      Into.Column := Column;
   end Set_Ident;

   --  ------------------------------
   --  Get the value token as an integer.
   --  ------------------------------
   function Get_Value (Token : in YYstype) return Natural is
   begin
      return Natural'Value (To_String (Token.Token));
   end Get_Value;

   --  ------------------------------
   --  Create a property rule.
   --  ------------------------------
   procedure Create_Property (Name : in YYstype;
                              Rule : in YYstype) is
   begin
      for Prop_Name of Name.Names loop
         if Prop_Name (Prop_Name'First) = '<' then
            Current_Repository.Create_Definition (Prop_Name, Rule.Rule);
         else
            Current_Repository.Create_Property (Prop_Name, Rule.Rule);
         end if;
      end loop;
   end Create_Property;

   --  ------------------------------
   --  Create a definition rule.
   --  ------------------------------
   procedure Create_Definition (Name : in YYstype;
                                Rule : in YYstype) is
   begin
      Current_Repository.Create_Definition (To_String (Name.Token), Rule.Rule);
   end Create_Definition;

   --  ------------------------------
   --  Create a type or a reference to a definition rule.
   --  ------------------------------
   procedure Create_Type_Or_Reference (Into : out YYstype;
                                       Name : in YYstype) is
      Loc : constant Rules.Location
         := Util.Log.Locations.Create_Line_Info (Current_File, Name.Line, Name.Column);
   begin
      Into := Name;
      Into.Rule := Current_Repository.Create_Definition (To_String (Name.Token), Loc);
   end Create_Type_Or_Reference;

   --  ------------------------------
   --  Create an identifier rule.
   --  ------------------------------
   procedure Create_Identifier (Into : out YYstype;
                                Name : in YYstype) is
      Loc : constant Rules.Location
         := Util.Log.Locations.Create_Line_Info (Current_File, Name.Line, Name.Column);
   begin
      Into := Name;
      Into.Rule := Rules.Create_Identifier (To_String (Name.Token), Loc);
   end Create_Identifier;

   --  ------------------------------
   --  Create an function with parameter rules.
   --  ------------------------------
   procedure Create_Function (Into   : out YYstype;
                              Name   : in YYstype;
                              Params : in YYstype) is
      Loc : constant Rules.Location
         := Util.Log.Locations.Create_Line_Info (Current_File, Name.Line, Name.Column);
   begin
      Into := Name;
      Into.Rule := Rules.Create_Function (To_String (Name.Token), Params.Rule, Loc);
   end Create_Function;

   --  Create a list of property names.
   procedure Create_Names (List : in out YYstype;
                           Name : in YYstype) is
   begin
      List.Names.Append (To_String (Name.Token));
   end Create_Names;

   procedure Append_Group (Into   : out YYstype;
                           Group  : in YYstype;
                           Item   : in YYstype;
                           Kind   : in Rules.Group_Type) is
   begin
      Into := Group;
      Rules.Append_Group (Into.Rule, Group.Rule, Item.Rule, Kind);
   end Append_Group;

   --  ------------------------------
   --  Report an error message while parsing the rule definition file.
   --  ------------------------------
   procedure Error (Line    : in Natural;
                    Column  : in Natural;
                    Message : in String) is
   begin
      Log.Error ("{0}:{1} {2}", Util.Log.Locations.Relative_Path (Current_File.all) & ":"
                 & Util.Strings.Image (Line), Util.Strings.Image (Column), Message);
   end Error;

end CSS.Analysis.Parser;
