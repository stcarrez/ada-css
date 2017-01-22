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

package CSS.Analysis.Rules.Types is

   type Integer_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents an integer without unit.
   overriding
   function Match (Rule  : in Integer_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Number_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents a number or integer without unit.
   overriding
   function Match (Rule  : in Number_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Percentage_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents a percentage.
   overriding
   function Match (Rule  : in Percentage_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Length_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents a length.
   overriding
   function Match (Rule  : in Length_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Angle_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents an angle.
   overriding
   function Match (Rule  : in Angle_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type String_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents a string.
   overriding
   function Match (Rule  : in String_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type URL_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents a string.
   overriding
   function Match (Rule  : in URL_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Color_Rule_Type is new Rule_Type with null record;

   --  Check if the value represents an hexadecimal color.
   overriding
   function Match (Rule  : in Color_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

end CSS.Analysis.Rules.Types;
