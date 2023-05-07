-----------------------------------------------------------------------
--  css-core-values -- Representation of CSS property value(s).
--  Copyright (C) 2017, 2018, 2023 Stephane Carrez
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
package body CSS.Core.Values is

   --  ------------------------------
   --  Get a printable representation of the value.
   --  ------------------------------
   function To_String (Value : in Value_Type) return String is
   begin
      if Value = null then
         return "null";
      elsif Value.Kind = VALUE_FUNCTION then
         return Value.Value & "(" & To_String (Value.Params.all, Sep => ',') & ")";
      else
         return Value.Value;
      end if;
   end To_String;

   --  ------------------------------
   --  Get the value type.
   --  ------------------------------
   function Get_Type (Value : in Value_Type) return Value_Kind is
   begin
      if Value = null then
         return VALUE_NULL;
      else
         return Value.Kind;
      end if;
   end Get_Type;

   --  ------------------------------
   --  Get the value unit.
   --  ------------------------------
   function Get_Unit (Value : in Value_Type) return Unit_Type is
   begin
      if Value = null then
         return UNIT_NONE;
      else
         return Value.Unit;
      end if;
   end Get_Unit;

   --  ------------------------------
   --  Get the value.
   --  ------------------------------
   function Get_Value (Value : in Value_Type) return String is
   begin
      if Value = null then
         return "";
      else
         return Value.Value;
      end if;
   end Get_Value;

   function "<" (Left, Right : in Value_Type) return Boolean is
   begin
      if Left = null then
         return False;
      elsif Right = null then
         return True;
      elsif Left.Kind /= Right.Kind then
         return Left.Kind < Right.Kind;
      elsif Left.Unit /= Right.Unit then
         return Left.Unit < Right.Unit;
      elsif Left.Kind /= VALUE_FUNCTION then
         return Left.Value < Right.Value;
      else
         return Left.Params.all < Right.Params.all;
      end if;
   end "<";

   function "<" (Left, Right : in Value_List) return Boolean is
      V1, V2 : Value_Type;
   begin
      if Left.Count < Right.Count then
         return True;
      elsif Left.Count > Right.Count then
         return False;
      else
         for I in 1 .. Left.Count loop
            V1 := Left.Get_Value (I);
            V2 := Right.Get_Value (I);
            if not Compare (V1, V2) then
               return V1 < V2;
            end if;
         end loop;
         return False;
      end if;
   end "<";

   --  ------------------------------
   --  Append the value to the list.
   --  ------------------------------
   procedure Append (List  : in out Value_List;
                     Value : in Value_Type) is
   begin
      for I in List.Values'Range loop
         if List.Values (I) = null then
            List.Values (I) := Value;
            List.Count := List.Count + 1;
            return;
         end if;
      end loop;
   end Append;

   --  ------------------------------
   --  Get the number of values in the list.
   --  ------------------------------
   function Get_Count (List : in Value_List) return Natural is
   begin
      return List.Count;
   end Get_Count;

   --  ------------------------------
   --  Get the value at the given list position.
   --  ------------------------------
   function Get_Value (List : in Value_List;
                       Pos  : in Positive) return Value_Type is
   begin
      return List.Values (Pos);
   end Get_Value;

   --  ------------------------------
   --  Get the function parameters.
   --  ------------------------------
   function Get_Parameters (Value : in Value_Type) return Value_List_Access is
   begin
      return Value.Params;
   end Get_Parameters;

   --  ------------------------------
   --  Get a printable representation of the list or a subset of the list.
   --  ------------------------------
   function To_String (List : in Value_List;
                       From : in Positive := 1;
                       To   : in Positive := Positive'Last;
                       Sep  : in Character := ' ') return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in From .. To loop
         exit when I > List.Count;
         if List.Values (I) /= null then
            if Ada.Strings.Unbounded.Length (Result) > 0 then
               Ada.Strings.Unbounded.Append (Result, Sep);
            end if;
            Ada.Strings.Unbounded.Append (Result, To_String (List.Values (I)));
         end if;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end To_String;

   --  ------------------------------
   --  Compare the two values for identity.
   --  ------------------------------
   function Compare (Left, Right : in Value_Type) return Boolean is
   begin
      if Left = Right then
         return True;
      end if;
      if Left = null or else Right = null then
         return False;
      end if;
      return Left.Kind = Right.Kind and then Left.Unit = Right.Unit
         and then Left.Len = Right.Len and then Left.Value = Right.Value;
   end Compare;

   function Create_Value (Repository : in out Repository_Type;
                          Value      : in Value_Type) return Value_Type is
      Pos : constant Value_Sets.Cursor := Repository.Root.Find (Value);
   begin
      if Value_Sets.Has_Element (Pos) then
         return Value_Sets.Element (Pos);
      end if;
      declare
         V : constant Value_Type := new Value_Node '(Len    => Value.Len,
                                                     Kind   => Value.Kind,
                                                     Unit   => Value.Unit,
                                                     Params => null,
                                                     Value  => Value.Value);
      begin
         Repository.Root.Insert (V);
         return V;
      end;
   end Create_Value;

   --  ------------------------------
   --  Create a color value.
   --  ------------------------------
   function Create_Color (Repository : in out Repository_Type;
                          Value      : in String) return Value_Type is
      V : aliased Value_Node := Value_Node '(Len    => Value'Length,
                                             Kind   => VALUE_COLOR,
                                             Unit   => UNIT_NONE,
                                             Params => null,
                                             Value  => Value);
   begin
      return Repository.Create_Value (V'Unchecked_Access);
   end Create_Color;

   --  ------------------------------
   --  Create a URL value.
   --  ------------------------------
   function Create_URL (Repository : in out Repository_Type;
                        Value      : in String) return Value_Type is
      V : aliased Value_Node := Value_Node '(Len    => Value'Length,
                                             Kind   => VALUE_URL,
                                             Unit   => UNIT_NONE,
                                             Params => null,
                                             Value  => Value);
   begin
      return Repository.Create_Value (V'Unchecked_Access);
   end Create_URL;

   --  ------------------------------
   --  Create a String value.
   --  ------------------------------
   function Create_String (Repository : in out Repository_Type;
                           Value      : in String) return Value_Type is
      V : aliased Value_Node := Value_Node '(Len    => Value'Length,
                                             Kind   => VALUE_STRING,
                                             Unit   => UNIT_NONE,
                                             Params => null,
                                             Value  => Value);
   begin
      return Repository.Create_Value (V'Unchecked_Access);
   end Create_String;

   --  ------------------------------
   --  Create an identifier value.
   --  ------------------------------
   function Create_Ident (Repository : in out Repository_Type;
                          Value      : in String) return Value_Type is
      V : aliased Value_Node := Value_Node '(Len    => Value'Length,
                                             Kind   => VALUE_IDENT,
                                             Unit   => UNIT_NONE,
                                             Params => null,
                                             Value  => Value);
   begin
      return Repository.Create_Value (V'Unchecked_Access);
   end Create_Ident;

   --  ------------------------------
   --  Create a number value with an optional unit.
   --  ------------------------------
   function Create_Number (Repository : in out Repository_Type;
                           Value      : in String;
                           Unit       : in Unit_Type := UNIT_NONE) return Value_Type is
      V : aliased Value_Node := Value_Node '(Len    => Value'Length,
                                             Kind   => VALUE_NUMBER,
                                             Unit   => Unit,
                                             Params => null,
                                             Value  => Value);
   begin
      return Repository.Create_Value (V'Unchecked_Access);
   end Create_Number;

   --  ------------------------------
   --  Create a function value with one parameter.
   --  ------------------------------
   function Create_Function (Repository : in out Repository_Type;
                             Name       : in String;
                             Parameter  : in Value_Type) return Value_Type is
      Params : Value_List;
   begin
      Append (Params, Parameter);
      return Repository.Create_Function (Name, Params);
   end Create_Function;

   --  ------------------------------
   --  Create a number value with an optional unit.
   --  ------------------------------
   --  Create a function value with parameters.
   function Create_Function (Repository : in out Repository_Type;
                             Name       : in String;
                             Parameters : in Value_List'Class) return Value_Type is
      Param : aliased Value_List := Value_List (Parameters);
      Check : aliased Value_Node := Value_Node '(Len   => Name'Length,
                                                 Kind  => VALUE_FUNCTION,
                                                 Unit  => UNIT_NONE,
                                                 Value => Name,
                                                 Params => Param'Unchecked_Access);
      Pos : constant Value_Sets.Cursor := Repository.Root.Find (Check'Unchecked_Access);
   begin
      if Value_Sets.Has_Element (Pos) then
         return Value_Sets.Element (Pos);
      end if;
      declare
         V : constant Value_Type
            := new Value_Node '(Len    => Name'Length,
                                Kind   => VALUE_FUNCTION,
                                Unit   => UNIT_NONE,
                                Value  => Name,
                                Params => new Value_List '(Value_List (Parameters)));
      begin
         Repository.Root.Insert (V);
         return V;
      end;
   end Create_Function;

   --  ------------------------------
   --  Return the number of entries in the repository.
   --  ------------------------------
   function Length (Repository : in Repository_Type) return Natural is
   begin
      return Natural (Repository.Root.Length);
   end Length;

   --  ------------------------------
   --  Iterate over the list of properties and call the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (Repository : in Repository_Type;
                      Process    : not null access procedure (Value : in Value_Type)) is
      procedure Process_Value (Pos : in CSS.Core.Values.Value_Sets.Cursor);
      procedure Process_Value (Pos : in CSS.Core.Values.Value_Sets.Cursor) is
      begin
         Process (CSS.Core.Values.Value_Sets.Element (Pos));
      end Process_Value;
   begin
      Repository.Root.Iterate (Process_Value'Access);
   end Iterate;

   --  ------------------------------
   --  Release the storage held by the selector sub-tree.
   --  ------------------------------
   procedure Finalize (Node : in out Value_Node) is
   begin
      null;
   end Finalize;

   --  ------------------------------
   --  Release the values held by the repository.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Repository_Type) is
   begin
      null;
   end Finalize;

   --  ------------------------------
   --  Copy the value list chain if there is one.
   --  ------------------------------
   overriding
   procedure Adjust (Object : in out Value_List) is
   begin
      if Object.Next /= null then
         null;
      end if;
   end Adjust;

   --  ------------------------------
   --  Release the values.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Value_List) is
   begin
      null;
   end Finalize;

end CSS.Core.Values;
