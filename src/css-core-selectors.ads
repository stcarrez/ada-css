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
private with Ada.Finalization;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Doubly_Linked_Lists;
package CSS.Core.Selectors is

   type CSSSelector_Tree is limited private;

   type Selector_Type is (SEL_CLASS,             --  .bold-class
                          SEL_ELEMENT,           --  div
                          SEL_IDENT,             --  #id-object
                          SEL_CHILD,             --  >
                          SEL_NEXT_SIBLING,      --  +
                          SEL_FOLLOWING_SIBLING, --  ~
                          SEL_PSEUDO_CLASS,      --  :visited
                          SEL_PSEUDO_ELEMENT,    --  :first-child
                          SEL_HAS_ATTRIBUTE,     --  [type]
                          SEL_EQ_ATTRIBUTE,      --  [type=checkbox]
                          SEL_MATCH_ATTRIBUTE,   --  [foo*="bar"]
                          SEL_STARTS_ATTRIBUTE,  --  [foo^="bar"]
                          SEL_ENDS_ATTRIBUTE,    --  [foo$="bar"]
                          SEL_CONTAIN_ATTRIBUTE, --  [foo~=val]
                          SEL_ORMATCH_ATTRIBUTE, --  [hreflang|="en"]
                          SEL_NOT,               --  :not(div > a)
                          SEL_HAS,               --  :has(h1, h2)
                          SEL_FUNCTION,          --  :matches(en, item)
                          SEL_PARAM,             --  Attribute/Function param
                          SEL_NONE,
                          SEL_COMBINE);

   type CSSSelector is private;

   --  Compare the two CSS selectors.
   function "<" (Left, Right : in CSSSelector) return Boolean;

   --  Compare the two CSS selectors.
   function "=" (Left, Right : in CSSSelector) return Boolean;

   --  Get a printable representation of the CSS selector.
   function To_String (Selector : in CSSSelector) return String;

   --  Create a CSS selector of the given type and with the name.
   function Create (Kind : in Selector_Type;
                    Name : in String) return CSSSelector;

   --  Create a CSS selector of the given type and with the name.
   function Create (Kind  : in Selector_Type;
                    Name  : in String;
                    Value : in String) return CSSSelector;

   --  Get the selector type for the first selector component.
   function Get_Selector_Type (Selector : in CSSSelector) return Selector_Type;

   --  Get the selector value for the first selector component.
   function Get_Value (Selector : in CSSSelector) return String;

   --  Append the selector at end of the selector list.
   procedure Append (Into     : in out CSSSelector;
                     Selector : in out CSSSelector);

   --  Append the selector at end of the selector list.
   procedure Append_Child (Into     : in out CSSSelector;
                           Selector : in out CSSSelector);

   --  Iterate over the list of CSS selector components.
   procedure Iterate (Selector : in CSSSelector;
                      Process  : not null access procedure (Sel : in CSSSelector));

   type CSSSelector_List is limited private;

   --  Append to the list of selectors the new selector component.
   procedure Append (Into     : in out CSSSelector_List;
                     Selector : in out CSSSelector);

   --  Return a printable representation of the CSS selector list.
   function To_String (List : in CSSSelector_List) return String;

   --  Iterate over the list of CSS selector.
   procedure Iterate (List    : in CSSSelector_List;
                      Process : not null access procedure (Sel : in CSSSelector));

private

   --  A selector node represents a filter element that must be matched
   --  by the given XML/HTML element.  The selector node are chained
   --  together when there are several XML/HTML elements in the path
   --  that must match.  The selector node can also have children
   --  selectors when more checks must be applied on a given element.
   --  The CSS selector:
   --
   --  div.item a:visited
   --
   --  is represented by 4 Selector_Node as follows:
   --
   --  [SEL_ELEMENT "div"]  -- Next -> [SEL_ELEMENT "a"]
   --      |                                |
   --    Child                            Child
   --      |                                |
   --      V                                V
   --  [SEL_CLASS ".item"]             [SEL_PSEUDO_CLASS "visited"]
   --
   --  This tree is not exposed outside of the package.
   --  The selector nodes are shared by selectors that have the same root definitions.
   --  The following CSS selector:
   --
   --  div.item input[type=checkbox]
   --
   --  uses the same root tree but the "input[type=checkbox]" alternative is represented
   --  by the sibling linked list.  With the two selectors, the tree looks as follows:
   --
   --  [SEL_ELEMENT "div"]  -- Next -> [SEL_ELEMENT "a"] -- Sibling --> [SEL_ELEMENT "input"]
   --      |                                |                                |
   --    Child                            Child                           Child
   --      |                                |                                |
   --      V                                V                                V
   --  [SEL_CLASS ".item"]             [SEL_PSEUDO_CLASS "visited"]     [SEL_EQ_ATTRIBUTE "type"]
   --                                                                        |
   --                                                                     Child
   --                                                                        |
   --                                                                        V
   --                                                                   [SEL_PARAM "checked"]
   --
   --  The <tt>CSSSelector</tt> object contains the Select_Node elements that must be matched.
   --  That is:
   --
   --  div.item a:visited is the array ( [SEL_ELEMENT "div"], [SEL_ELEMENT "a"] )
   --  div.item input[type=checkbox] is the array ( [SEL_ELEMENT "div"], [SEL_ELEMENT "input"])
   --
   type Selector_Node;
   type Selector_Node_Access is access all Selector_Node;
   type Selector_Node (Len  : Natural;
                       Kind : Selector_Type) is
   limited record
      Child   : Selector_Node_Access;
      Sibling : Selector_Node_Access;
      Parent  : Selector_Node_Access;
      Next    : Selector_Node_Access;
      Params  : Selector_Node_Access;
      Value   : String (1 .. Len);
   end record;

   --  Compare the two selectors to order them.
   function "<" (Left, Right : Selector_Node) return Boolean;

   --  Compare the two selectors for identity.
   function "=" (Left, Right : Selector_Node) return Boolean;

   --  Release the storage held by the selector sub-tree.
   procedure Finalize (Selector : in out Selector_Node);

   type Selector_Tree_Node;
   type Selector_Tree_Node_Access is access all Selector_Tree_Node;

   type Selector_Tree_Node (Len  : Natural;
                            Kind : Selector_Type) is
   limited record
      Sibling  : Selector_Tree_Node_Access;
      Parent   : Selector_Tree_Node_Access;
      Child    : Selector_Tree_Node_Access;
      Next     : Selector_Tree_Node_Access;
      Selector : aliased Selector_Node (Len, Kind);
   end record;

   --  Compare the two selectors to order them.
   function "<" (Left, Right : Selector_Tree_Node_Access) return Boolean;

   --  Compare the two selectors to check if they are equal.
   --  We only check the first selector.
   function Compare (Left, Right : Selector_Tree_Node_Access) return Boolean;

   --  The Node_Sets package is used by the CSS selector tree to allow searching
   --  for selectors in an efficient manner.
   package Node_Sets is
      new Ada.Containers.Ordered_Sets (Element_Type => Selector_Tree_Node_Access,
                                       "<"          => "<",
                                       "="          => Compare);

   type Selector_Node_Access_Array is array (Positive range <>) of Selector_Node_Access;

   type CSSSelector is new Ada.Finalization.Controlled with record
      Sel : Selector_Node_Access_Array (1 .. 5);
   end record;

   type CSSSelector_Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Sets.Set;
   end record;

   --  Release the selector objects that have been allocated in the tree.
   overriding
   procedure Finalize (Tree : in out CSSSelector_Tree);

   package Selector_List is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => CSSSelector);

   type CSSSelector_List is limited record
      List : Selector_List.List;
   end record;

end CSS.Core.Selectors;
