-----------------------------------------------------------------------
--  css-core-medias -- CSS rule to represent a media rule
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
with CSS.Core.Vectors;
with Util.Strings.Vectors;
package CSS.Core.Medias is

   type CSSMediaRule is new CSS.Core.Groups.CSSGroupingRule with record
      Medias : Util.Strings.Vectors.Vector;
   end record;
   type CSSMediaRule_Access is access all CSSMediaRule'Class;

   --  Get the type that identifies the rule.
   overriding
   function Get_Type (Rule : in CSSMediaRule) return CSSRule_Type;

end CSS.Core.Medias;
