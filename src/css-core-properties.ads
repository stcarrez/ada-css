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

package CSS.Core.Properties is

   --  Exception raised when a property cannot be found.
   NO_PROPERTY : exception;

   type CSSProperty is record
      Rule     : CSSRule_Access;
      Name     : CSSProperty_Name;
      Value    : CSSProperty_Value;
      Location : CSS.Core.Location;
   end record;

   type CSSProperty_List is tagged limited private;

   --  Get the property with the given name from the list.
   --  If the property is defined several times, the last value
   --  that was inserted is returned.
   --  Raises the <tt>NO_PROPERTY</tt> exception if the property was not found.
   function Get_Property (List : in CSSProperty_List;
                          Name : in String) return CSSProperty;

   --  Append the CSS property with the value to the list.
   procedure Append (List  : in out CSSProperty_List;
                     Name  : in CSSProperty_Name;
                     Value : in CSSProperty_Value;
                     Line  : in Natural := 0);

private

   type CSSInternal_Property is record
      Name     : CSSProperty_Name;
      Value    : CSSProperty_Value;
      Line     : Natural;
   end record;

   type CSSInternal_Property_Array is array (Positive range <>) of CSSInternal_Property;
   type CSSInternal_Property_Array_Access is access all CSSInternal_Property_Array;

   type CSSProperty_List is new Ada.Finalization.Limited_Controlled with record
      Parent     : CSSRule_Access;
      Properties : CSSInternal_Property_Array_Access;
   end record;

   --  Release the memory used by the property list.
   overriding
   procedure Finalize (List : in out CSSProperty_List);

end CSS.Core.Properties;
