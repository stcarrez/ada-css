-----------------------------------------------------------------------
--  css-analysis-classes -- CSS Class Analysis
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
with Ada.Containers.Indefinite_Ordered_Maps;

with CSS.Core.Errors;
with CSS.Core.Sets;
with CSS.Core.Selectors;
with CSS.Core.Sheets;
with CSS.Tools.Location_Sets;

--  == Analysis of CSS Classes ==
--  The <tt>CSS.Analysis.Classes</tt> package defines some simple analysis
--  arround the CSS classes defined and used by the CSS rule selectors.
package CSS.Analysis.Classes is

   type Class_Information is record
      Name : CSS.Core.Selectors.CSSSelector;
      List : CSS.Core.Sets.Set;
      Loc  : CSS.Tools.Location_Sets.Set;
   end record;

   package Class_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                  Element_Type => Class_Information);
   subtype Map is Class_Maps.Map;
   subtype Cursor is Class_Maps.Cursor;

   --  Analyze the CSS selectors used by the stylesheet and collect the class
   --  information with the associated CSS rules.
   procedure Analyze (Sheet   : in CSS.Core.Sheets.CSSStylesheet;
                      Result  : in out Class_Maps.Map;
                      Report  : in out CSS.Core.Errors.Error_Handler'Class);

end CSS.Analysis.Classes;
