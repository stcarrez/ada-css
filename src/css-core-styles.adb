-----------------------------------------------------------------------
--  css-core-styles -- Core CSS API definition
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

package body CSS.Core.Styles is

   --  ------------------------------
   --  Get the type that identifies the rule.
   --  ------------------------------
   overriding
   function Get_Type (Rule : in CSSStyleRule) return CSSRule_Type is
      pragma Unreferenced (Rule);
   begin
      return STYLE_RULE;
   end Get_Type;

   --  ------------------------------
   --  If the reference is valid and represents a style rule, return it.
   --  Otherwise returns null.
   --  ------------------------------
   function Value (Ref : in CSS.Core.Refs.Ref) return CSSStyleRule_Access is
   begin
      if Ref.Is_Null then
         return null;
      else
         declare
            V : constant CSS.Core.CSSRule_Access := Ref.Value;
         begin
            if V.all in CSSStyleRule'Class then
               return CSSStyleRule'Class (V.all)'Access;
            else
               return null;
            end if;
         end;
      end if;
   end Value;

   --  ------------------------------
   --  If the cursor is valid and represents a style rule, return it.
   --  Otherwise returns null.
   --  ------------------------------
   function Element (Pos : in CSS.Core.Vectors.Cursor) return CSSStyleRule_Access is
   begin
      return Value (CSS.Core.Vectors.Element (Pos));
   end Element;

   --  ------------------------------
   --  Get the type that identifies the rule.
   --  ------------------------------
   overriding
   function Get_Type (Rule : in CSSPageRule) return CSSRule_Type is
      pragma Unreferenced (Rule);
   begin
      return PAGE_RULE;
   end Get_Type;

   --  ------------------------------
   --  Get the type that identifies the rule.
   --  ------------------------------
   overriding
   function Get_Type (Rule : in CSSFontfaceRule) return CSSRule_Type is
      pragma Unreferenced (Rule);
   begin
      return FONT_FACE_RULE;
   end Get_Type;

end CSS.Core.Styles;
