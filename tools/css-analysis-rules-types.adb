-----------------------------------------------------------------------
--  css-analysis-rules-types -- Rules for CSS pre-defined value types
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

package body CSS.Analysis.Rules.Types is

   use CSS.Core.Values;

   --  ------------------------------
   --  Print the rule definition to the print stream.
   --  ------------------------------
   procedure Print (Rule   : in Builtin_Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class) is
   begin
      Stream.Print (Ada.Strings.Unbounded.To_String (Rule.Name));
   end Print;

   --  ------------------------------
   --  Check if the value represents an integer without unit.
   --  ------------------------------
   overriding
   function Match (Rule  : in Integer_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) = UNIT_NONE;
   end Match;

   --  ------------------------------
   --  Check if the value represents a number or integer without unit.
   --  ------------------------------
   overriding
   function Match (Rule  : in Number_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) = UNIT_NONE;
   end Match;

   --  ------------------------------
   --  Check if the value represents a percentage.
   --  ------------------------------
   overriding
   function Match (Rule  : in Percentage_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) = UNIT_NONE;
   end Match;

   --  ------------------------------
   --  Check if the value represents a length.
   --  ------------------------------
   overriding
   function Match (Rule  : in Length_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER
         and (Get_Unit (Value) in Length_Unit_Type or Get_Unit (Value) = UNIT_NONE);
   end Match;

   --  ------------------------------
   --  Check if the value represents a time.
   --  ------------------------------
   overriding
   function Match (Rule  : in Time_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) in Time_Unit_Type;
   end Match;

   --  ------------------------------
   --  Check if the value represents a resolution.
   --  ------------------------------
   overriding
   function Match (Rule  : in Resolution_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) = UNIT_PI;
   end Match;

   --  ------------------------------
   --  Check if the value represents an angle.
   --  ------------------------------
   overriding
   function Match (Rule  : in Angle_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) in Angle_Unit_Type;
   end Match;

   --  ------------------------------
   --  Check if the value represents a string.
   --  ------------------------------
   overriding
   function Match (Rule  : in String_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_STRING;
   end Match;

   --  ------------------------------
   --  Check if the value represents a url.
   --  ------------------------------
   overriding
   function Match (Rule  : in URL_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_URL;
   end Match;

   --  ------------------------------
   --  Check if the value represents an hexadecimal color.
   --  ------------------------------
   overriding
   function Match (Rule  : in Color_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_COLOR;
   end Match;

   --  ------------------------------
   --  Check if the value represents a custom identifier.
   --  ------------------------------
   overriding
   function Match (Rule  : in Identifier_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return Get_Type (Value) = VALUE_IDENT;
   end Match;

   --  ------------------------------
   --  Register the builtin type in the repository.
   --  ------------------------------
   procedure Register_Builtin (Repository : in out Repository_Type;
                               Name       : in String;
                               Rule       : in Builtin_Rule_Type_Access;
                               Kind       : in CSS.Core.Values.Value_Kind) is
   begin
      Rule.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Repository.Types.Insert (Name, Rule.all'Access);
   end Register_Builtin;

   Int_Rule        : aliased Types.Integer_Rule_Type;
   Percent_Rule    : aliased Types.Percentage_Rule_Type;
   Length_Rule     : aliased Types.Length_Rule_Type;
   Number_Rule     : aliased Types.Number_Rule_Type;
   Angle_Rule      : aliased Types.Angle_Rule_Type;
   String_Rule     : aliased Types.String_Rule_Type;
   URL_Rule        : aliased Types.URL_Rule_Type;
   Color_Rule      : aliased Types.Color_Rule_Type;
   Ident_Rule      : aliased Types.Identifier_Rule_Type;
   Resolution_Rule : aliased Types.Resolution_Rule_Type;
   Time_Rule       : aliased Types.Time_Rule_Type;

   procedure Register (Repository : in out Repository_Type) is
   begin
      Register_Builtin (Repository, "<angle>", Angle_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<integer>", Int_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<number>", Number_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<length>", Length_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<percentage>", Percent_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<string>", String_Rule'Access, VALUE_STRING);
      Register_Builtin (Repository, "<url>", URL_Rule'Access, VALUE_URL);
      Register_Builtin (Repository, "<hex-color>", Color_Rule'Access, VALUE_COLOR);
      Register_Builtin (Repository, "<custom-ident>", Ident_Rule'Access, VALUE_IDENT);
      Register_Builtin (Repository, "<resolution>", Resolution_Rule'Access, VALUE_NUMBER);
      Register_Builtin (Repository, "<time>", Time_Rule'Access, VALUE_NUMBER);
   end Register;

end CSS.Analysis.Rules.Types;
