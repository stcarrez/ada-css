-----------------------------------------------------------------------
--  css -- Ada CSS Library
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
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Util.Log.Loggers;
with CSS.Parser.Parser;
package body CSS.Parser is

   use Ada.Strings.Unbounded;
   use Util.Concurrent.Counters;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("CSS.Parser");

   procedure Load (Path  : in String;
                   Sheet : in CSS.Core.Stylesheet_Access) is
      Res : Integer := CSS.Parser.Parser.Parse (Path, Sheet);
   begin
      null;
   end Load;

   function To_String (Val : in Parser_Node_Access) return String is
   begin
      if Val = null then
         return "null";
      end if;
      case Val.Kind is
         when TYPE_STRING | TYPE_IDENT =>
            return Ada.Strings.Unbounded.To_String (Val.Str_Value);

         when TYPE_STYLE =>
            return "<style>";

         when TYPE_PROPERTY =>
            return To_String (Val.Name) & ": " & To_String (Val.Value);

         when others =>
            return "";

      end case;
   end To_String;

   --  ------------------------------
   --  Return a printable representation of the parse value.
   --  ------------------------------
   function To_String (Val : in YYstype) return String is
   begin
      case Val.Kind is
         when TYPE_NULL =>
            return "null";

         when TYPE_STRING | TYPE_IDENT =>
            return To_String (Val.Node);

         when TYPE_URI =>
            return "url(" & To_String (Val.Node) & ")";

         when TYPE_VALUE =>
            return To_String (Val.Node);

         when others =>
            return "?";

      end case;
   end To_String;

   procedure Set_Type (Into   : in out YYstype;
                       Kind   : in Node_Type;
                       Line   : in Natural;
                       Column : in Natural) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Parser_Node_Type, Parser_Node_Access);

      Release : Boolean;
   begin
      if Into.Node /= null then
         Util.Concurrent.Counters.Decrement (Into.Node.Ref_Counter, Release);
         if Release then
            Free (Into.Node);
         else
            Into.Node := null;
         end if;
      end if;
      Into.Kind   := Kind;
      Into.Line   := Line;
      Into.Column := Column;
   end Set_Type;

   --  ------------------------------
   --  Set the parser token with a string that represent an identifier.
   --  The line and column number are recorded in the token.
   --  ------------------------------
   procedure Set_Ident (Into   : in out YYstype;
                        Value  : in String;
                        Line   : in Natural;
                        Column : in Natural) is
   begin
      Set_Type (Into, TYPE_IDENT, Line, Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_IDENT,
                                          Ref_Counter => ONE,
                                          others      => <>);
      Ada.Strings.Unbounded.Set_Unbounded_String (Into.Node.Str_Value, Value);
   end Set_Ident;

   --  ------------------------------
   --  Set the parser token with a string.
   --  The line and column number are recorded in the token.
   --  ------------------------------
   procedure Set_String (Into   : in out YYstype;
                         Value  : in String;
                         Line   : in Natural;
                         Column : in Natural) is
   begin
      Set_Type (Into, TYPE_STRING, Line, Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_STRING,
                                          Ref_Counter => ONE,
                                          others      => <>);
      Ada.Strings.Unbounded.Set_Unbounded_String (Into.Node.Str_Value, Value);
   end Set_String;

   --  ------------------------------
   --  Set the parser token with a url string.
   --  The line and column number are recorded in the token.
   --  ------------------------------
   procedure Set_Uri (Into   : in out YYstype;
                      Value  : in String;
                      Line   : in Natural;
                      Column : in Natural) is
   begin
      Set_Type (Into, TYPE_URI, Line, Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_URI,
                                          Ref_Counter => ONE,
                                          others      => <>);
      Ada.Strings.Unbounded.Set_Unbounded_String (Into.Node.Str_Value, Value);
   end Set_Uri;

   --  ------------------------------
   --  Set the parser token with an number value with an optional unit or dimension.
   --  The line and column number are recorded in the token.
   --  ------------------------------
   procedure Set_Number (Into   : in out YYstype;
                         Value  : in String;
                         Unit   : in Unit_Type;
                         Line   : in Natural;
                         Column : in Natural) is
   begin
      Set_Type (Into, TYPE_VALUE, Line, Column);
      Into.Unit := Unit;
      Into.Kind := TYPE_VALUE;
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_STRING,
                                          Ref_Counter => ONE,
                                          others      => <>);
      Ada.Strings.Unbounded.Set_Unbounded_String (Into.Node.Str_Value, Value);
   end Set_Number;

   --  ------------------------------
   --  Set the parser token to represent a property identifier and its value expression.
   --  The value may be a multi-value (ex: 1px 2em 3 4).  The priority indicates whether
   --  the !important keyword was present.
   --  ------------------------------
   procedure Set_Property (Into  : in out YYstype;
                           Ident : in YYstype;
                           Value : in YYstype;
                           Prio  : in Boolean) is
   begin
      Set_Type (Into, TYPE_PROPERTY, Ident.Line, Ident.Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_PROPERTY,
                                          Ref_Counter => ONE,
                                          Name        => Ident.Node,
                                          Value       => Value.Node,
                                          Prio        => Prio);
      Util.Concurrent.Counters.Increment (Ident.Node.Ref_Counter);
      Util.Concurrent.Counters.Increment (Value.Node.Ref_Counter);
   end Set_Property;

   --  ------------------------------
   --  Set the parser token to represent a list of properties held by a CSSStyleRule
   --  declaration.  The style rule is created and the first property inserted in it.
   --  The stylesheet document is used for the property string allocation.
   --  ------------------------------
   procedure Set_Property_List (Into     : in out YYstype;
                                Document : in CSS.Core.Stylesheet_Access;
                                Prop     : in YYstype) is
   begin
      Set_Type (Into, TYPE_STYLE, Prop.Line, Prop.Column);
      Into.Node := new Parser_Node_Type '(Kind => TYPE_STYLE, Ref_Counter => ONE, Rule => null);
      Into.Node.Rule := new CSS.Core.Styles.CSSStyleRule;
      Append_Property (Into.Node.Rule.Style, Document, Prop);
   end Set_Property_List;

   function Get_Property_Name (Document : in CSS.Core.Stylesheet_Access;
                               Prop     : in YYstype) return CSS.Core.CSSProperty_Name is
   begin
      if Prop.Node = null then
         return null;
      else
         return Document.Create_Property_Name (To_String (Prop.Node.Name));
      end if;
   end Get_Property_Name;

   function Get_Property_Value (Document : in CSS.Core.Stylesheet_Access;
                                Prop     : in YYstype) return CSS.Core.CSSProperty_Name is
   begin
      if Prop.Node = null then
         return null;
      else
         return Document.Create_Property_Name (To_String (Prop.Node.Value));
      end if;
   end Get_Property_Value;

   --  ------------------------------
   --  Append to the CSSStyleRule the property held by the parser token.
   --  ------------------------------
   procedure Append_Property (Into     : in out CSS.Core.Styles.CSSStyle_Declaration;
                              Document : in CSS.Core.Stylesheet_Access;
                              Prop     : in YYstype) is
      Name  : CSS.Core.CSSProperty_Name := Get_Property_Name (Document, Prop);
      Value : CSS.Core.CSSProperty_Value := Get_Property_Value (Document, Prop);
   begin
      Into.Append (Name, null, 0);
   end Append_Property;

   --  ------------------------------
   --  Set the parser token to represent the CSS selector list.
   --  The first selector searched in the document, inserted in the document
   --  CSS selector tree and then added to the selector list.
   --  ------------------------------
   procedure Set_Selector_List (Into     : in out YYstype;
                                Document : in CSS.Core.Stylesheet_Access;
                                Selector : in YYstype) is
   begin
      Log.Error ("Selector '{0}'", CSS.Core.Selectors.To_String (Selector.Node.Selector));
   end Set_Selector_List;

   --  ------------------------------
   --  Append to the CSS selector list the selector.  The selector is first
   --  searched in the document CSS selector tree and inserted in the tree.
   --  It is then added to the list.
   --  ------------------------------
   procedure Add_Selector_List (Into     : in out CSS.Core.Styles.CSSStyleRule_Access;
                                Document : in CSS.Core.Stylesheet_Access;
                                Selector : in YYstype) is
      use type CSS.Core.Styles.CSSStyleRule_Access;
   begin
      if Into = null then
         Into := new CSS.Core.Styles.CSSStyleRule;
      end if;
      CSS.Core.Selectors.Append (Into.Selectors, Selector.Node.Selector);
   end Add_Selector_List;

   --  ------------------------------
   --  Set the parser token to represent the CSS selector.
   --  ------------------------------
   procedure Set_Selector (Into     : in out YYstype;
                           Selector : in YYstype) is
   begin
      Set_Type (Into, TYPE_SELECTOR, Selector.Line, Selector.Column);
   end Set_Selector;

   --  ------------------------------
   --  Set the parser token to represent the CSS selector.
   --  ------------------------------
   procedure Set_Selector (Into     : in out YYstype;
                           Kind     : in CSS.Core.Selectors.Selector_Type;
                           Selector : in YYstype) is
   begin
      Set_Type (Into, TYPE_SELECTOR, Selector.Line, Selector.Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_SELECTOR,
                                          Ref_Counter => ONE,
                                          Selector    => <>);
      Into.Node.Selector := CSS.Core.Selectors.Create (Kind, To_String (Selector));
   end Set_Selector;

   --  ------------------------------
   --  Set the parser token to represent the CSS selector.
   --  ------------------------------
   procedure Set_Selector (Into     : in out YYstype;
                           Kind     : in CSS.Core.Selectors.Selector_Type;
                           Param1   : in YYstype;
                           Param2   : in YYstype) is
   begin
      Set_Type (Into, TYPE_SELECTOR, Param1.Line, Param1.Column);
      Into.Node := new Parser_Node_Type '(Kind        => TYPE_SELECTOR,
                                          Ref_Counter => ONE,
                                          Selector    => <>);
      Into.Node.Selector := CSS.Core.Selectors.Create (Kind, To_String (Param1), To_String (Param2));
   end Set_Selector;

   --  ------------------------------
   --  Add to the current parser token CSS selector the next CSS selector.
   --  ------------------------------
   procedure Add_Selector (Into     : in out YYstype;
                           Selector : in YYstype) is
   begin
      CSS.Core.Selectors.Append (Into.Node.Selector, Selector.Node.Selector);
   end Add_Selector;

   --  ------------------------------
   --  Add to the parser token CSS selector a filter represented either
   --  by an attribute selection, a pseudo element, a pseudo class or
   --  a function.
   --  ------------------------------
   procedure Add_Selector_Filter (Into   : in out YYstype;
                                  Filter : in YYstype) is
   begin
      CSS.Core.Selectors.Append_Child (Into.Node.Selector, Filter.Node.Selector);
   end Add_Selector_Filter;

   --  ------------------------------
   --  Set the parser token to represent a CSS selector type.
   --  Record the line and column where the selector type is found.
   --  ------------------------------
   procedure Set_Selector_Type (Into     : in out YYstype;
                                Selector : in CSS.Core.Selectors.Selector_Type;
                                Line     : in Natural;
                                Column   : in Natural) is
   begin
      Set_Type (Into, TYPE_SELECTOR_TYPE, Line, Column);
      Into.Sel := Selector;
   end Set_Selector_Type;

   procedure Set_Expr (Into  : in out YYstype;
                       Left  : in YYstype;
                       Right : in YYstype) is
   begin
      Log.Error ("Expression {0} {1}", To_String (Left), To_String (Right));
   end Set_Expr;

   procedure Set_Expr (Into  : in out YYstype;
                       Left  : in YYstype;
                       Oper  : in YYstype;
                       Right : in YYstype) is
   begin
      Log.Error ("Expression {0} {1} {2}", To_String (Left), To_String (Oper), To_String (Right));
   end Set_Expr;

   procedure Set_Function (Into   : in out YYstype;
                           Name   : in YYstype;
                           Params : in YYstype) is
   begin
      null;
   end Set_Function;

   procedure Error (Line    : in Natural;
                    Column  : in Natural;
                    Message : in String) is
      L : constant String := Natural'Image (Line);
      C : constant String := Natural'Image (Column);
   begin
      Ada.Text_IO.Put_Line (L (L'First + 1 .. L'Last) & ":"
                            & C (C'First + 1 .. C'Last) & ": " & Message);
   end Error;

   overriding
   procedure Adjust (Object : in out YYstype) is
   begin
      if Object.Node /= null then
         Util.Concurrent.Counters.Increment (Object.Node.Ref_Counter);
      end if;
   end Adjust;
   
   overriding
   procedure Finalize (Object : in out YYstype) is
   begin
      Set_Type (Object, TYPE_NULL, 0, 0);
   end Finalize;

end CSS.Parser;
