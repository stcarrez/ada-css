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
                          SEL_EQ_ATTRIBUTE,      --  [type=checkbox]
                          SEL_MATCH_ATTRIBUTE,   --  [foo*="bar"]
                          SEL_STARTS_ATTRIBUTE,  --  [foo^="bar"]
                          SEL_ENDS_ATTRIBUTE,    --  [foo$="bar"]
                          SEL_CONTAIN_ATTRIBUTE, --  [foo~=val]
                          SEL_ORMATCH_ATTRIBUTE, --  [hreflang|="en"]
                          SEL_FUNCTION,          --  :not(div > a)
                          SEL_COMBINE);

   type CSSSelector is private;

   --  Create a CSS selector of the given type and with the name.
   function Create (Kind : in Selector_Type;
                    Name : in String) return CSSSelector;

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
   --  Is represented by 4 Selector_Node as follows:
   --
   --  [SEL_ELEMENT "div"]  -- Next -> [SEL_ELEMENT "a"]
   --      |                                |
   --    Child                            Child
   --      |                                |
   --      V                                V 
   --  [SEL_CLASS ".item"]             [SEL_PSEUDO_CLASS "visited"]
   --
   --  This tree is not exposed outside of the package.
   type Selector_Node;
   type Selector_Node_Access is access all Selector_Node;
   type Selector_Node (Len  : Natural;
                       Kind : Selector_Type) is limited record
      Child   : Selector_Node_Access;
      Next    : Selector_Node_Access;
      Value   : String (1 .. Len);
   end record;

   type Selector_Tree_Node;
   type Selector_Tree_Node_Access is access all Selector_Tree_Node;

   type Selector_Tree_Node (Len  : Natural;
                            Kind : Selector_Type) is limited record
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

   type CSSSelector is new Ada.Finalization.Controlled with record
      Sel : Selector_Node_Access;
   end record;

   type CSSSelector_Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Sets.Set;
   end record;

end CSS.Core.Selectors;
