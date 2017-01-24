-----------------------------------------------------------------------
--  css-core-sheets -- CSS stylesheet representation
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

with CSS.Core.Styles;
with CSS.Core.Vectors;
with CSS.Core.Values;
with CSS.Core.Properties;
with CSS.Core.Medias;
package CSS.Core.Sheets is

   type CSSStylesheet is new CSS.Core.StyleSheet with record
      Rules  : CSS.Core.Vectors.Vector;
      Values : CSS.Core.Values.Repository_Type;
   end record;
   type CSSStylesheet_Access is access all CSSStylesheet'Class;

   --  Create a CSS rule.
   function Create_Rule (Document : in CSSStyleSheet) return Styles.CSSStyleRule_Access;

   --  Create a CSS font-face rule.
   function Create_Rule (Document : in CSSStyleSheet) return Styles.CSSFontfaceRule_Access;

   --  Create a CSS media rule.
   function Create_Rule (Document : in CSSStyleSheet) return Medias.CSSMediaRule_Access;

   --  Append the CSS rule to the document.
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Styles.CSSStyleRule_Access;
                     Line     : in Natural;
                     Column   : in Natural);

   --  Append the media rule to the document.
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Medias.CSSMediaRule_Access;
                     Line     : in Natural;
                     Column   : in Natural);

   --  Append the font-face rule to the document.
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Styles.CSSFontfaceRule_Access;
                     Line     : in Natural;
                     Column   : in Natural);

   --  Append the CSS rule to the media.
   procedure Append (Document : in out CSSStylesheet;
                     Media    : in Medias.CSSMediaRule_Access;
                     Rule     : in Styles.CSSStyleRule_Access;
                     Line     : in Natural;
                     Column   : in Natural);

   --  Iterate over the properties of each CSS rule.  The <tt>Process</tt> procedure
   --  is called with the CSS rule and the property as parameter.
   procedure Iterate_Properties (Document : in CSSStylesheet;
                                 Process  : not null access
                                   procedure (Rule     : in Styles.CSSStyleRule'Class;
                                              Property : in Properties.CSSProperty));

end CSS.Core.Sheets;
