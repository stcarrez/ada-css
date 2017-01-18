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
with Util.Strings;
with CSS.Analysis.Parser.Parser;
with CSS.Core;
package body CSS.Analysis.Parser is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Load the rule definition file and populate the rule repository.
   --  ------------------------------
   procedure Load (Path : in String) is
      Res : Integer;
   begin
      Log.Info ("Loading rule definition file {0}", Path);
      Res := CSS.Analysis.Parser.Parser.Parse (Path);
      if Res /= 0 then
         Log.Error ("Found {0} errors while parsing {1}", Util.Strings.Image (Res), Path);
      end if;
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
      Rules.Rule_Repository.Create_Property (To_String (Name.Token), Rule.Rule);
   end Create_Property;

   --  ------------------------------
   --  Create a definition rule.
   --  ------------------------------
   procedure Create_Definition (Name : in YYstype;
                                Rule : in YYstype) is
   begin
      Rules.Rule_Repository.Create_Definition (To_String (Name.Token), Rule.Rule);
   end Create_Definition;

   --  ------------------------------
   --  Create a type or a reference to a definition rule.
   --  ------------------------------
   procedure Create_Type_Or_Reference (Into : out YYstype;
                                       Name : in YYstype) is
      Loc : CSS.Core.Location;
   begin
      Into := Name;
      Into.Rule := Rules.Rule_Repository.Create_Definition (To_String (Name.Token), Loc);
   end Create_Type_Or_Reference;

   --  ------------------------------
   --  Create an identifier rule.
   --  ------------------------------
   procedure Create_Identifier (Into : out YYstype;
                                Name : in YYstype) is
      Loc : CSS.Core.Location;
   begin
      Into := Name;
      Into.Rule := Rules.Create_Identifier (To_String (Name.Token), Loc);
   end Create_Identifier;

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
      Log.Error ("{0}:{1}: {2}", Util.Strings.Image (Line), Util.Strings.Image (Column), Message);
   end Error;

end CSS.Analysis.Parser;