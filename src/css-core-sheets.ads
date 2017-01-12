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

with CSS.Core.Groups;
with CSS.Core.Styles;
with CSS.Core.Refs;
with CSS.Core.Vectors;
package CSS.Core.Sheets is

   type CSSStyleSheet is new CSS.Core.StyleSheet with record
      Rules : CSS.Core.Vectors.Vector;
   end record;
   type CSSStyleSheet_Access is access all CSSStyleSheet'Class;

   --  Create a CSS rule.
   function Create_Rule (Document : in CSSStyleSheet) return Styles.CSSStyleRule_Access;

   procedure Append (Document : in out CSSStyleSheet;
                     Rule     : in Styles.CSSStyleRule_Access);

end CSS.Core.Sheets;
