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
      return Get_Type (Value) = VALUE_NUMBER and Get_Unit (Value) in Length_Unit_Type;
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

end CSS.Analysis.Rules.Types;
