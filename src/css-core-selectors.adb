-----------------------------------------------------------------------
--  css-core-selectors -- Core CSS API definition
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
package body CSS.Core.Selectors is

   --  ------------------------------
   --  Create a CSS selector of the given type and with the name.
   --  ------------------------------
   function Create (Kind : in Selector_Type;
                    Name : in String) return CSSSelector is
      Result : CSSSelector;
      Node   : constant Selector_Node_Access
         := new Selector_Node '(Len   => Name'Length,
                                Kind  => Kind,
                                Value => Name,
                                Next  => null,
                                Child => null);
   begin
      Result.Sel := Node;
      return Result;
   end Create;

   --  ------------------------------
   --  Append the selector at end of the selector list.
   --  ------------------------------
   procedure Append (Into     : in out CSSSelector;
                     Selector : in out CSSSelector) is
      Last : Selector_Node_Access := Into.Sel;
   begin
      if Last = null then
         Into.Sel := Selector.Sel;
      else
         while Last.Next /= null loop
            Last := Last.Next;
         end loop;
         Last.Next := Selector.Sel;
      end if;
      Selector.Sel := null;
   end Append;

   --  ------------------------------
   --  Compare the two selectors to order them.
   --  ------------------------------
   function "<" (Left, Right : Selector_Tree_Node_Access) return Boolean is
   begin
      if Left = null then
         return True;
      end if;
      if Right = null then
         return False;
      end if;
      if Left.Kind < Right.Kind then
         return True;
      elsif Left.Kind > Right.Kind then
         return False;
      else
         return Left.Selector.Value < Right.Selector.Value;
      end if;
   end "<";

   --  ------------------------------
   --  Compare the two selectors to check if they are equal.
   --  We only check the first selector.
   --  ------------------------------
   function Compare (Left, Right : Selector_Tree_Node_Access) return Boolean is
   begin
      if Left = Right then
         return True;
      end if;
      if Left = null or else Right = null then
         return False;
      end if;
      return Left.Kind = Right.Kind and Left.Selector.Value = Right.Selector.Value;
   end Compare;

end CSS.Core.Selectors;
