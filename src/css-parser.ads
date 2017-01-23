-----------------------------------------------------------------------
--  css-parser -- Ada CSS Parser
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
with Ada.Strings.Unbounded;
with CSS.Core;
with CSS.Core.Sheets;
with CSS.Core.Errors;
private with CSS.Core.Selectors;
private with Util.Concurrent.Counters;
private with CSS.Core.Styles;
private with CSS.Core.Medias;
private with CSS.Core.Values;
private with Ada.Finalization;
package CSS.Parser is

   procedure Load (Path    : in String;
                   Sheet   : in CSS.Core.Sheets.CSSStylesheet_Access;
                   Handler : in CSS.Core.Errors.Error_Handler_Access);

   --  The parser token or expression.
   type YYstype is private;

   --  Return a printable representation of the parser token value.
   function To_String (Val : in YYstype) return String;

   --  Empty token.
   EMPTY : constant YYstype;

private

   type Node_Type is (TYPE_NULL, TYPE_VALUE, TYPE_NUMBER, TYPE_COLOR,
                      TYPE_STRING, TYPE_URI, TYPE_IDENT, TYPE_STYLE, TYPE_PROPERTY, TYPE_SELECTOR,
                      TYPE_SELECTOR_TYPE,
                      TYPE_PROPERTY_LIST, TYPE_ERROR, TYPE_ADD, TYPE_APPEND);

   --  Set the parser token with a string.
   --  The line and column number are recorded in the token.
   procedure Set_String (Into   : in out YYstype;
                         Value  : in String;
                         Line   : in Natural;
                         Column : in Natural);

   --  Append the token as a string.
   procedure Append_String (Into   : in out YYstype;
                            Value  : in YYstype);
   procedure Append_String (Into   : in out YYstype;
                            Value  : in String);
   procedure Append_String (Into   : in out YYstype;
                            Value1 : in YYstype;
                            Value2 : in YYstype);

   --  Set the parser token with a string that represent an identifier.
   --  The line and column number are recorded in the token.
   procedure Set_Ident (Into   : in out YYstype;
                        Value  : in String;
                        Line   : in Natural;
                        Column : in Natural);

   --  Set the parser token with an number value with an optional unit or dimension.
   --  The line and column number are recorded in the token.
   procedure Set_Number (Into   : in out YYstype;
                         Value  : in String;
                         Unit   : in CSS.Core.Values.Unit_Type;
                         Line   : in Natural;
                         Column : in Natural);

   --  Set the parser token with a url string.
   --  The line and column number are recorded in the token.
   procedure Set_Uri (Into   : in out YYstype;
                      Value  : in String;
                      Line   : in Natural;
                      Column : in Natural);

   --  Set the parser token with a color.
   --  Report an error if the color is invalid.
   procedure Set_Color (Into  : in out YYstype;
                        Value : in YYstype);

   --  Set the parser token to represent a property identifier and its value expression.
   --  The value may be a multi-value (ex: 1px 2em 3 4).  The priority indicates whether
   --  the !important keyword was present.
   procedure Set_Property (Into  : in out YYstype;
                           Ident : in YYstype;
                           Value : in YYstype;
                           Prio  : in Boolean);

   --  Set the parser token to represent a list of properties held by a CSSStyleRule
   --  declaration.  The style rule is created and the first property inserted in it.
   --  The stylesheet document is used for the property string allocation.
   procedure Set_Property_List (Into     : in out YYstype;
                                Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                                Prop     : in YYstype);

   --  Append to the CSSStyleRule the property held by the parser token.
   procedure Append_Property (Into     : in out CSS.Core.Styles.CSSStyle_Declaration;
                              Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                              Prop     : in YYstype);
   procedure Append_Property (Into     : in out CSS.Core.Styles.CSSStyleRule_Access;
                              Media    : in CSS.Core.Medias.CSSMediaRule_Access;
                              Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                              Prop     : in YYstype);

   procedure Append_Media (Into     : in out CSS.Core.Medias.CSSMediaRule_Access;
                           Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                           List     : in YYstype);

   --  Set the parser token to represent the CSS selector list.
   --  The first selector searched in the document, inserted in the document
   --  CSS selector tree and then added to the selector list.
   procedure Set_Selector_List (Into     : in out YYstype;
                                Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                                Selector : in YYstype);

   --  Append to the CSS selector list the selector.  The selector is first
   --  searched in the document CSS selector tree and inserted in the tree.
   --  It is then added to the list.
   procedure Add_Selector_List (Into     : in out CSS.Core.Styles.CSSStyleRule_Access;
                                Media    : in CSS.Core.Medias.CSSMediaRule_Access;
                                Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                                Selector : in YYstype);

   --  Set the parser token to represent the CSS selector.
   procedure Set_Selector (Into     : in out YYstype;
                           Selector : in YYstype);

   --  Set the parser token to represent the CSS selector.
   procedure Set_Selector (Into     : in out YYstype;
                           Kind     : in CSS.Core.Selectors.Selector_Type;
                           Selector : in YYstype);

   --  Set the parser token to represent the CSS selector.
   procedure Set_Selector (Into     : in out YYstype;
                           Kind     : in CSS.Core.Selectors.Selector_Type;
                           Param1   : in YYstype;
                           Param2   : in YYstype);

   --  Add to the current parser token CSS selector the next CSS selector.
   procedure Add_Selector (Into     : in out YYstype;
                           Selector : in YYstype);

   --  Add to the parser token CSS selector a filter represented either
   --  by an attribute selection, a pseudo element, a pseudo class or
   --  a function.
   procedure Add_Selector_Filter (Into   : in out YYstype;
                                  Filter : in YYstype);

   --  Set the parser token to represent a CSS selector type.
   --  Record the line and column where the selector type is found.
   procedure Set_Selector_Type (Into     : in out YYstype;
                                Selector : in CSS.Core.Selectors.Selector_Type;
                                Line     : in Natural;
                                Column   : in Natural);

   procedure Set_Value (Into     : in out YYstype;
                        Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                        Value    : in YYstype);

   procedure Set_Expr (Into  : in out YYstype;
                       Left  : in YYstype;
                       Right : in YYstype);

   procedure Set_Expr (Into  : in out YYstype;
                       Left  : in YYstype;
                       Oper  : in YYstype;
                       Right : in YYstype);

   procedure Set_Function (Into   : in out YYstype;
                           Name   : in YYstype;
                           Params : in YYstype);

   type Parser_Node_Type;
   type Parser_Node_Access is access all Parser_Node_Type;
   type Parser_Node_Type (Kind : Node_Type) is limited record
      Ref_Counter : Util.Concurrent.Counters.Counter;
      case Kind is
         when TYPE_STRING | TYPE_IDENT | TYPE_URI | TYPE_NUMBER | TYPE_COLOR =>
            Str_Value : Ada.Strings.Unbounded.Unbounded_String;

         when TYPE_STYLE  =>
            Rule : CSS.Core.Styles.CSSStyleRule_Access;

         when TYPE_PROPERTY =>
            Name  : Parser_Node_Access;
            Value : Parser_Node_Access;
            Prio  : Boolean := False;

         when TYPE_SELECTOR =>
            Selector : CSS.Core.Selectors.CSSSelector;

         when TYPE_VALUE =>
            V : CSS.Core.Values.Value_Type;

         when TYPE_PROPERTY_LIST =>
            Values : CSS.Core.Values.Value_List;

         when others =>
            null;

      end case;
   end record;

   function Create_Value (Document : in CSS.Core.Sheets.CSSStylesheet_Access;
                          From     : in YYstype) return CSS.Core.Values.Value_Type;

   function To_String (Val : in Parser_Node_Access) return String;

   type YYstype is new Ada.Finalization.Controlled with record
      Line     : Natural    := 0;
      Column   : Natural    := 0;
      Unit     : CSS.Core.Values.Unit_Type  := CSS.Core.Values.UNIT_NONE;
      Kind     : Node_Type := TYPE_NULL;
      Sel      : CSS.Core.Selectors.Selector_Type := CSS.Core.Selectors.SEL_CLASS;
      Node     : Parser_Node_Access;
   end record;

   overriding
   procedure Adjust (Object : in out YYstype);

   overriding
   procedure Finalize (Object : in out YYstype);

   procedure Error (Line    : in Natural;
                    Column  : in Natural;
                    Message : in String);

   procedure Warning (Line    : in Natural;
                      Column  : in Natural;
                      Message : in String);

   EMPTY : constant YYstype := YYstype '(Ada.Finalization.Controlled with
                                         Line => 0, Column => 0,
                                         Unit => CSS.Core.Values.UNIT_NONE,
                                         Kind => TYPE_NULL,
                                         Sel  => CSS.Core.Selectors.SEL_CLASS,
                                         Node => null);

end CSS.Parser;
