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
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
package body CSS.Core.Selectors is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Build a string representation of the selector sub-tree.
   --  ------------------------------
   procedure To_String (Into     : in out Ada.Strings.Unbounded.Unbounded_String;
                        Selector : in Selector_Node_Access) is
      Node : Selector_Node_Access := Selector;
   begin
      if Node /= null then
         case Node.Kind is
            when SEL_CLASS | SEL_ELEMENT | SEL_IDENT =>
               Append (Into, Node.Value);
            when SEL_CHILD =>
               Append (Into, ">");
            when SEL_NEXT_SIBLING =>
               Append (Into, "+");
            when SEL_FOLLOWING_SIBLING =>
               Append (Into, "~");
            when SEL_PSEUDO_ELEMENT =>
               Append (Into, "::");
               Append (Into, Node.Value);
            when SEL_PSEUDO_CLASS =>
               Append (Into, ":");
               Append (Into, Node.Value);
            when SEL_HAS_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "]");
            when SEL_EQ_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_MATCH_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "*=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_STARTS_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "^=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_ENDS_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "$=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_CONTAIN_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "~=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_ORMATCH_ATTRIBUTE =>
               Append (Into, "[");
               Append (Into, Node.Value);
               Append (Into, "|=");
               Append (Into, Node.Params.Value);
               Append (Into, "]");
            when SEL_FUNCTION =>
               Append (Into, ":");
               Append (Into, Node.Value);
               Append (Into, "(");
               --  Append (Into, Node.Params.Value);
               Append (Into, ")");
            when others =>
               null;
         end case;
         if Node.Child /= null then
            To_String (Into, Node.Child);
         end if;
      end if;
   end To_String;

   procedure To_String (Into     : in out Ada.Strings.Unbounded.Unbounded_String;
                        Selector : in CSSSelector) is
   begin
      for I in Selector.Sel'Range loop
         exit when Selector.Sel (I) = null;
         if I > Selector.Sel'First then
            Append (Into, " ");
         end if;
         To_String (Into, Selector.Sel (I));
      end loop;
   end To_String;

   --  ------------------------------
   --  Get a printable representation of the CSS selector.
   --  ------------------------------
   function To_String (Selector : in CSSSelector) return String is
      Result : Unbounded_String;
   begin
      To_String (Result, Selector);
      return To_String (Result);
   end To_String;

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
                                Parent => null,
                                Sibling => null,
                                Params  => null,
                                Next  => null,
                                Child => null);
   begin
      Result.Sel (1) := Node;
      return Result;
   end Create;

   --  ------------------------------
   --  Create a CSS selector of the given type and with the name.
   --  ------------------------------
   function Create (Kind  : in Selector_Type;
                    Name  : in String;
                    Value : in String) return CSSSelector is
      Result : CSSSelector := Create (Kind, Name);
      Node   : constant Selector_Node_Access
         := new Selector_Node '(Len     => Value'Length,
                                Kind    => SEL_PARAM,
                                Value   => Value,
                                Parent  => Result.Sel (1),
                                Sibling => null,
                                Params  => null,
                                Next    => null,
                                Child   => null);
   begin
      Result.Sel (1).Params := Node;
      return Result;
   end Create;

   --  ------------------------------
   --  Get the selector type for the first selector component.
   --  ------------------------------
   function Get_Selector_Type (Selector : in CSSSelector) return Selector_Type is
   begin
      if Selector.Sel (1) = null then
         return SEL_NONE;
      else
         return Selector.Sel (1).Kind;
      end if;
   end Get_Selector_Type;

   --  ------------------------------
   --  Append the selector at end of the selector list.
   --  ------------------------------
   procedure Append (Into     : in out CSSSelector;
                     Selector : in out CSSSelector) is
      Last : Selector_Node_Access := null; --  Into.Sel (1);
   begin
      for I in Into.Sel'Range loop
         if Into.Sel (I) = null then
            Into.Sel (I) := Selector.Sel (1);
            exit when Last = null;
            Into.Sel (I).Parent := Last;
            while Last.Next /= null loop
               Last := Last.Next;
            end loop;
            Last.Next := Selector.Sel (1);
            exit;
         end if;
         Last := Into.Sel (I);
      end loop;
      Selector.Sel (1) := null;
   end Append;

   --  ------------------------------
   --  Append the selector at end of the selector list.
   --  ------------------------------
   procedure Append_Child (Into     : in out CSSSelector;
                           Selector : in out CSSSelector) is
      Sel : Selector_Node_Access := Into.Sel (1);
   begin
      while Sel.Child /= null loop
         Sel := Sel.Child;
      end loop;
      Sel.Child := Selector.Sel (1);
      Sel.Child.Parent := Sel;
      Selector.Sel (1) := null;
   end Append_Child;

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

   --  ------------------------------
   --  Release the storage held by the selector sub-tree.
   --  ------------------------------
   procedure Finalize (Selector : in out Selector_Node) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Selector_Node,
                                         Selector_Node_Access);
   begin
      if Selector.Child /= null then
         Finalize (Selector.Child.all);
         Free (Selector.Child);
      end if;
      if Selector.Next /= null then
         Finalize (Selector.Next.all);
         Free (Selector.Next);
      end if;
   end Finalize;

   procedure Finalize (Tree : in out Selector_Tree_Node_Access) is
   begin
      if Tree.Child /= null then
         Finalize (Tree.Child);
      end if;
      if Tree.Next /= null then
         Finalize (Tree.Next);
      end if;
   end Finalize;

   --  Release the selector objects that have been allocated in the tree.
   overriding
   procedure Finalize (Tree : in out CSSSelector_Tree) is
   begin
      null;
   end Finalize;

   --  ------------------------------
   --  Append to the list of selectors the new selector component.
   --  ------------------------------
   procedure Append (Into     : in out CSSSelector_List;
                     Selector : in out CSSSelector) is
   begin
      Into.List.Append (Selector);
   end Append;

   --  ------------------------------
   --  Return a printable representation of the CSS selector list.
   --  ------------------------------
   function To_String (List : in CSSSelector_List) return String is
      Result : Unbounded_String;
      Iter   : Selector_List.Cursor := List.List.First;
   begin
      while Selector_List.Has_Element (Iter) loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         To_String (Result, Selector_List.Element (Iter));
         Selector_List.Next (Iter);
      end loop;
      return To_String (Result);
   end To_String;

end CSS.Core.Selectors;
