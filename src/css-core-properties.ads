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
private with Ada.Finalization;
with Util.Log.Locations;
with CSS.Core.Values;

package CSS.Core.Properties is

   --  Exception raised when a property cannot be found.
   NO_PROPERTY : exception;

   subtype Value_Type is CSS.Core.Values.Value_Type;
   subtype Value_List is CSS.Core.Values.Value_List;
   use type CSS.Core.Values.Value_List;

   type CSSProperty is record
      Rule     : CSSRule_Access;
      Name     : CSSProperty_Name;
      Value    : Value_List;
      Location : CSS.Core.Location;
   end record;

   type CSSProperty_List is tagged limited private;

   --  Get the property with the given name from the list.
   --  If the property is defined several times, the last value
   --  that was inserted is returned.
   --  Raises the <tt>NO_PROPERTY</tt> exception if the property was not found.
   function Get_Property (List : in CSSProperty_List;
                          Name : in String) return CSSProperty;

   --  Get the number of properties in the list.
   function Get_Length (List : in CSSProperty_List) return Natural;

   --  Returns true if the two property list are identical.  They contain
   --  the same properties in the same order.
   function "=" (Left, Right : in CSSProperty_List) return Boolean;

   --  Set the file information associated with the property list.
   procedure Set_File_Info (Into : in out CSSProperty_List;
                            File : in Util.Log.Locations.File_Info_Access);

   --  Append the CSS property with the value to the list.
   procedure Append (List   : in out CSSProperty_List;
                     Name   : in CSSProperty_Name;
                     Value  : in Value_List;
                     Line   : in Natural := 0;
                     Column : in Natural := 0);

   --  Append the CSS property with the value to the list.
   procedure Append (List   : in out CSSProperty_List;
                     Name   : in CSSProperty_Name;
                     Value  : in Value_Type;
                     Line   : in Natural := 0;
                     Column : in Natural := 0);

   --  Iterate over the list of properties and call the <tt>Process</tt> procedure.
   procedure Iterate (List    : in CSSProperty_List;
                      Process : not null access procedure (Prop : in CSSProperty));

private

   type CSSInternal_Property is record
      Name     : CSSProperty_Name;
      Value    : Value_List;
      Line     : Natural;
      Column   : Natural;
   end record;

   type CSSInternal_Property_Array is array (Positive range <>) of CSSInternal_Property;
   type CSSInternal_Property_Array_Access is access all CSSInternal_Property_Array;

   type CSSProperty_List is new Ada.Finalization.Limited_Controlled with record
      Parent     : CSSRule_Access;
      File       : Util.Log.Locations.File_Info_Access;
      Properties : CSSInternal_Property_Array_Access;
   end record;

   --  Release the memory used by the property list.
   overriding
   procedure Finalize (List : in out CSSProperty_List);

end CSS.Core.Properties;
