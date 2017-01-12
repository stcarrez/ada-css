-----------------------------------------------------------------------
--  css-core-groups -- CSS rule to represent a group of CSS rules
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

with CSS.Core.Vectors;
package CSS.Core.Groups is

   type CSSGroupingRule is new CSS.Core.CSSRule with record
      Rules : CSS.Core.Vectors.Vector;
   end record;
   type CSSGroupingRule_Access is access all CSSGroupingRule'Class;

   --  Get the type that identifies the rule.
   overriding
   function Get_Type (Rule : in CSSGroupingRule) return CSSRule_Type;

end CSS.Core.Groups;
