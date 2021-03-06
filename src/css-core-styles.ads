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

with CSS.Core.Properties;
with CSS.Core.Selectors;
with CSS.Core.Refs;
with CSS.Core.Vectors;
package CSS.Core.Styles is

   type CSSStyle_Declaration is new CSS.Core.Properties.CSSProperty_List with null record;

   type CSSStyleRule is new CSS.Core.CSSRule with record
      Selectors : CSS.Core.Selectors.CSSSelector_List;
      Style     : CSSStyle_Declaration;
   end record;
   type CSSStyleRule_Access is access all CSSStyleRule'Class;

   --  Get the type that identifies the rule.
   overriding
   function Get_Type (Rule : in CSSStyleRule) return CSSRule_Type;

   --  If the reference is valid and represents a style rule, return it.
   --  Otherwise returns null.
   function Value (Ref : in CSS.Core.Refs.Ref) return CSSStyleRule_Access;

   --  If the cursor is valid and represents a style rule, return it.
   --  Otherwise returns null.
   function Element (Pos : in CSS.Core.Vectors.Cursor) return CSSStyleRule_Access;

   type CSSPageRule is new CSS.Core.CSSRule with record
      Selector  : CSS.Core.Selectors.CSSSelector;
      Style     : CSSStyle_Declaration;
   end record;
   type CSSPageRule_Access is access all CSSPageRule'Class;

   --  Get the type that identifies the rule.
   overriding
   function Get_Type (Rule : in CSSPageRule) return CSSRule_Type;

   type CSSFontfaceRule is new CSS.Core.CSSRule with record
      Style     : CSSStyle_Declaration;
   end record;
   type CSSFontfaceRule_Access is access all CSSFontfaceRule'Class;

   --  Get the type that identifies the rule.
   overriding
   function Get_Type (Rule : in CSSFontfaceRule) return CSSRule_Type;

end CSS.Core.Styles;
