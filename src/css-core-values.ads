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

--  == CSS Values ==
--  The <tt>CSS.Core.Values</tt> package describes types and operations to hold CSS property
--  values.  A property value is a list of tokens some of them being strings, urls, integers,
--  typed integers (for example dimensions) or function calls.  Each of these token is
--  represented by the <tt>Value_Type</tt> type and the property value is represented by
--  the <tt>Value_List</tt>.
--
--  The value token is created and maintained in the <tt>Repository_Type</tt> that holds all the
--  possible values.
package CSS.Core.Values is

   --  The optional value token unit.  For example, 12px has the unit <tt>UNIT_PX</tt>.
   type Unit_Type is (UNIT_NONE,
                      UNIT_PERCENT,
                      UNIT_PX,
                      UNIT_EX,
                      UNIT_EM,
                      UNIT_REM,  --  relative font-size of the root element
                      UNIT_LH,   --  relative to line height of the element
                      UNIT_RLH,  --  relative to the line height of the root element
                      UNIT_IC,   --  average character advance of a fullwidth glyph
                      UNIT_VW,   --  relative to 1% of the width of the viewport
                      UNIT_VH,   --  relative to 1% of the height of the viewport
                      UNIT_VI,   --  1% of viewport’s size in the root element’s inline axis
                      UNIT_VB,   --  1% of viewport’s size in the root element’s block axis
                      UNIT_VMIN, --  1% of viewport’s smaller dimension
                      UNIT_VMAX, --  1% of viewport’s larger dimension
                      UNIT_CM, UNIT_MM, UNIT_IN,
                      UNIT_DPI,
                      UNIT_DPCM,
                      UNIT_DPPX,
                      UNIT_PI, UNIT_PC, UNIT_PT,
                      UNIT_DEG, UNIT_RAD, UNIT_GRAD,
                      UNIT_MS, UNIT_SEC, UNIT_HZ, UNIT_KHZ);

   subtype Length_Unit_Type is Unit_Type range UNIT_PX .. UNIT_PT;
   subtype Time_Unit_Type is Unit_Type range UNIT_MS .. UNIT_SEC;
   subtype Angle_Unit_Type is Unit_Type range UNIT_DEG .. UNIT_GRAD;

   --  The type of the value.
   type Value_Kind is (VALUE_NULL,
                       VALUE_IDENT,    --  Identifier: left, right, ...
                       VALUE_STRING,   --  String: "..." or '...'
                       VALUE_URL,      --  URL: url(http://...) or url("../img.png")
                       VALUE_NUMBER,   --  Number: .2em or 10
                       VALUE_COLOR,    --  Color: #fed or rgb(10, 20, 30)
                       VALUE_FUNCTION);

   type Value_List;
   type Value_List_Access is access all Value_List;

   type Value_Type is private;

   --  Get a printable representation of the value.
   function To_String (Value : in Value_Type) return String;

   --  Get the value type.
   function Get_Type (Value : in Value_Type) return Value_Kind;

   --  Get the value unit.
   function Get_Unit (Value : in Value_Type) return Unit_Type;

   --  Get the value.
   function Get_Value (Value : in Value_Type) return String;

   --  Get the function parameters.
   function Get_Parameters (Value : in Value_Type) return Value_List_Access;

   function "<" (Left, Right : in Value_Type) return Boolean;

   EMPTY : constant Value_Type;

   type Value_List is tagged private;

   function "<" (Left, Right : in Value_List) return Boolean;

   --  Append the value to the list.
   procedure Append (List  : in out Value_List;
                     Value : in Value_Type);

   --  Get the number of values in the list.
   function Get_Count (List : in Value_List) return Natural;

   --  Get the value at the given list position.
   function Get_Value (List : in Value_List;
                       Pos  : in Positive) return Value_Type;

   --  Get a printable representation of the list or a subset of the list.
   function To_String (List : in Value_List;
                       From : in Positive := 1;
                       To   : in Positive := Positive'Last;
                       Sep  : in Character := ' ') return String;

   --  Compare the two values for identity.
   function Compare (Left, Right : in Value_Type) return Boolean;

   EMPTY_LIST : constant Value_List;

   --  A repository of all known values.
   type Repository_Type is new Ada.Finalization.Limited_Controlled with private;

   --  Create a color value.
   function Create_Color (Repository : in out Repository_Type;
                          Value      : in String) return Value_Type;

   --  Create a URL value.
   function Create_URL (Repository : in out Repository_Type;
                        Value      : in String) return Value_Type;

   --  Create a String value.
   function Create_String (Repository : in out Repository_Type;
                           Value      : in String) return Value_Type;

   --  Create an identifier value.
   function Create_Ident (Repository : in out Repository_Type;
                          Value      : in String) return Value_Type;

   --  Create a function value with one parameter.
   function Create_Function (Repository : in out Repository_Type;
                             Name       : in String;
                             Parameter  : in Value_Type) return Value_Type;

   --  Create a function value with parameters.
   function Create_Function (Repository : in out Repository_Type;
                             Name       : in String;
                             Parameters : in Value_List'Class) return Value_Type;

   --  Create a number value with an optional unit.
   function Create_Number (Repository : in out Repository_Type;
                           Value      : in String;
                           Unit       : in Unit_Type := UNIT_NONE) return Value_Type;

   --  Return the number of entries in the repository.
   function Length (Repository : in Repository_Type) return Natural;

private

   type Value_Node;
   type Value_Type is access all Value_Node;
   type Value_Node (Len  : Natural;
                    Kind : Value_Kind) is
   limited record
      Unit    : Unit_Type;
      Params  : Value_List_Access;
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

   type Repository_Type is new Ada.Finalization.Limited_Controlled with record
      Root : Value_Sets.Set;
   end record;

   function Create_Value (Repository : in out Repository_Type;
                          Value      : in Value_Type) return Value_Type;

   --  Release the values held by the repository.
   overriding
   procedure Finalize (Object : in out Repository_Type);

   type Value_Array is array (Positive range <>) of Value_Type;

   type Value_List is new Ada.Finalization.Controlled with record
      Next   : Value_List_Access;
      Values : Value_Array (1 .. 10);
      Count  : Natural := 0;
   end record;

   --  Copy the value list chain if there is one.
   overriding
   procedure Adjust (Object : in out Value_List);

   --  Release the values.
   overriding
   procedure Finalize (Object : in out Value_List);

   EMPTY      : constant Value_Type := null;
   EMPTY_LIST : constant Value_List := Value_List '(Ada.Finalization.Controlled with
                                                    Next   => null,
                                                    Values => (others => null),
                                                    Count  => 0);

end CSS.Core.Values;
