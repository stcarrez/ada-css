-----------------------------------------------------------------------
--  css-core-values -- Representation of CSS property value(s).
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
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;
package CSS.Core.Values is

   type Unit_Type is (UNIT_NONE, UNIT_PX, UNIT_EX, UNIT_EM,
                      UNIT_CM, UNIT_MM, UNIT_IN, UNIT_PI, UNIT_PC, UNIT_PT,
                      UNIT_DEG, UNIT_RAD, UNIT_GRAD,
                      UNIT_MS, UNIT_SEC, UNIT_HZ, UNIT_KHZ);

   type Value_Kind is (VALUE_IDENT, VALUE_STRING, VALUE_URL, VALUE_COLOR);

   type Value_Type is private;

   --  Get a printable representation of the value.
   function To_String (Value : in Value_Type) return String;

   --  Get the value type.
   function Get_Type (Value : in Value_Type) return Value_Kind;

   --  Get the value unit.
   function Get_Unit (Value : in Value_Type) return Unit_Type;

   function "<" (Left, Right : in Value_Type) return Boolean;

   type Value_List is private;

   --  Compare the two values for identity.
   function Compare (Left, Right : in Value_Type) return Boolean;

   --  A repository of all known values.
   type Repository is new Ada.Finalization.Limited_Controlled with private;

   --  Create a CSS selector of the given type and with the name.
   function Create_Color (Name : in String) return Value_Type;

private

   type Value_Node;
   type Value_Type is access all Value_Node;
   type Value_Node (Len  : Natural;
                    Kind : Value_Type) is limited record
      Unit    : Unit_Type;
      Value   : String (1 .. Len);
   end record;

   --  The Value_Sets package represents a set of values all of
   --  them being strictly different.
   package Value_Sets is
      new Ada.Containers.Ordered_Sets (Element_Type => Value_Type,
                                       "<"          => "<",
                                       "="          => Compare);

   --  Release the storage held by the selector sub-tree.
   procedure Finalize (Node : in out Value_Node);

   type Repository is new Ada.Finalization.Limited_Controlled with record
      Root : Value_Sets.Set;
   end record;

   --  Release the selector objects that have been allocated in the tree.
   overriding
   procedure Finalize (Object : in out Repository);

   type Value_Array is array (Positive range <>) of Value_Type;
   type Value_List_Access is access all Value_List;

   type Value_List is new Ada.Finalization.Controlled with record
      Next   : Value_List_Access;
      Values : Value_Array (1 .. 4);
   end record;

end CSS.Core.Values;
