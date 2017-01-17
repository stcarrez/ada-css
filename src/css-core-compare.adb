-----------------------------------------------------------------------
--  css-core-compare -- Comparision on CSS rule references
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

package body CSS.Core.Compare is

   function "=" (Left, Right : in CSS.Core.Refs.Ref) return Boolean is
   begin
      return Left.Value = Right.Value;
   end "=";

   --  ------------------------------
   --  Compare the two rules to order them.  Rules are compared on their
   --  source location.  The comparison is intended to be used by the
   --  <tt>CSS.Core.Sets</tt> package to allow the creation of sets that
   --  contain unique rules.
   --  ------------------------------
   function "<" (Left, Right : in CSS.Core.Refs.Ref) return Boolean is
      Left_Rule  : constant CSSRule_Access := Left.Value;
      Right_Rule : constant CSSRule_Access := Right.Value;
   begin
      if Left_Rule = null then
         return False;
      elsif Right_Rule = null then
         return True;
      else
         return Left_Rule.Get_Location < Right_Rule.Get_Location;
      end if;
   end "<";

end CSS.Core.Compare;
