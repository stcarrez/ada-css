-----------------------------------------------------------------------
--  css-core-properties -- Core CSS API definition
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
pragma Ada_2012;
with Ada.Unchecked_Deallocation;
package body CSS.Core.Properties is

   procedure Free is
      new Ada.Unchecked_Deallocation (CSSInternal_Property_Array,
                                      CSSInternal_Property_Array_Access);

   --  ------------------------------
   --  Get the property with the given name from the list.
   --  If the property is defined several times, the last value
   --  that was inserted is returned.
   --  Raises the <tt>NO_PROPERTY</tt> exception if the property was not found.
   --  ------------------------------
   function Get_Property (List : in CSSProperty_List;
                          Name : in String) return CSSProperty is
   begin
      if List.Properties /= null then
         for P of List.Properties.all loop
            if P.Name.all = Name then
               return Result : CSSProperty do
                  Result.Name  := P.Name;
                  Result.Value := P.Value;
                  Result.Rule  := List.Parent;
                  Result.Location.Line := P.Line;
               end return;
            end if;
         end loop;
      end if;
      raise NO_PROPERTY with "CSS property '" & Name & "' not found";
   end Get_Property;

   --  ------------------------------
   --  Get the number of properties in the list.
   --  ------------------------------
   function Get_Length (List : in CSSProperty_List) return Natural is
   begin
      if List.Properties = null then
         return 0;
      else
         return List.Properties'Length;
      end if;
   end Get_Length;

   --  ------------------------------
   --  Returns true if the two property list are identical.  They contain
   --  the same properties in the same order.
   --  ------------------------------
   function "=" (Left, Right : in CSSProperty_List) return Boolean is
   begin
      if Left.Properties = Right.Properties then
         return True;
      elsif Left.Properties = null or else Right.Properties = null then
         return False;
      elsif Left.Properties'Length /= Right.Properties'Length then
         return False;
      else
         for I in Left.Properties'Range loop
            --  Compare only the property name and property value.
            --  (we don't care about the line and column).
            if Left.Properties (I).Name /= Right.Properties (I).Name then
               return False;
            end if;
            if Left.Properties (I).Value /= Right.Properties (I).Value then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   --  ------------------------------
   --  Append the CSS property with the value to the list.
   --  ------------------------------
   procedure Append (List  : in out CSSProperty_List;
                     Name  : in CSSProperty_Name;
                     Value : in Value_List;
                     Line  : in Natural := 0) is
      New_List : CSSInternal_Property_Array_Access;
   begin
      if List.Properties = null then
         New_List := new CSSInternal_Property_Array (1 .. 1);
      else
         New_List := new CSSInternal_Property_Array (1 .. List.Properties'Length + 1);
         New_List (List.Properties'Range) := List.Properties.all;
      end if;
      New_List (New_List'Last).Name  := Name;
      New_List (New_List'Last).Value := Value;
      New_List (New_List'Last).Line  := Line;
      Free (List.Properties);
      List.Properties := New_List;
   end Append;

   --  ------------------------------
   --  Append the CSS property with the value to the list.
   --  ------------------------------
   procedure Append (List  : in out CSSProperty_List;
                     Name  : in CSSProperty_Name;
                     Value : in Value_Type;
                     Line  : in Natural := 0) is
      New_List : CSSInternal_Property_Array_Access;
   begin
      if List.Properties = null then
         New_List := new CSSInternal_Property_Array (1 .. 1);
      else
         New_List := new CSSInternal_Property_Array (1 .. List.Properties'Length + 1);
         New_List (List.Properties'Range) := List.Properties.all;
      end if;
      New_List (New_List'Last).Name  := Name;
      New_List (New_List'Last).Value.Append (Value);
      New_List (New_List'Last).Line  := Line;
      Free (List.Properties);
      List.Properties := New_List;
   end Append;


   --  ------------------------------
   --  Iterate over the list of properties and call the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (List    : in CSSProperty_List;
                      Process : not null access procedure (Prop : in CSSProperty)) is
      Prop : CSSProperty;
   begin
      if List.Properties /= null then
         Prop.Rule := List.Parent;
         for P of List.Properties.all loop
            Prop.Name  := P.Name;
            Prop.Value := P.Value;
            Prop.Location.Line  := P.Line;
            Process (Prop);
         end loop;
      end if;
   end Iterate;

   --  ------------------------------
   --  Release the memory used by the property list.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out CSSProperty_List) is
   begin
      Free (List.Properties);
   end Finalize;

end CSS.Core.Properties;
