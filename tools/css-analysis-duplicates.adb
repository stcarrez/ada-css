-----------------------------------------------------------------------
--  css-analysis-duplicates -- Duplicate CSS Rule Analysis
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
with CSS.Core.Refs;
with CSS.Core.Styles;
with CSS.Core.Properties;
with CSS.Core.Selectors;
package body CSS.Analysis.Duplicates is

   --  ------------------------------
   --  Analyze the CSS rules defined in the vectors to check for duplicate rules.
   --  Report a warning for each rule that appears to be a duplicate of another.
   --  Collect in <tt>Dups</tt> the set of rules which are duplicates.
   --  The initial <tt>Dups</tt> set can already contain existing rules.
   --  ------------------------------
   procedure Analyze (Rules  : in CSS.Core.Vectors.Vector;
                      Report : in out CSS.Core.Errors.Error_Handler'Class;
                      Dups   : in out CSS.Core.Sets.Set) is
      procedure Process (Pos : in CSS.Core.Vectors.Cursor);

      use type CSS.Core.CSSRule_Type;
      use CSS.Core.Properties;
      use CSS.Core.Styles;

      procedure Process (Pos : in CSS.Core.Vectors.Cursor) is
         Rule : constant CSSStyleRule_Access := Element (Pos);
         Iter : CSS.Core.Vectors.Cursor;
      begin
         if Rule = null or else Dups.Contains (CSS.Core.Vectors.Element (Pos)) then
            return;
         end if;
         Iter := CSS.Core.Vectors.Next (Pos);
         while CSS.Core.Vectors.Has_Element (Iter) loop
            declare
               Ref   : constant CSS.Core.Refs.Ref := CSS.Core.Vectors.Element (Iter);
               Match : constant CSSStyleRule_Access := Element (Iter);
            begin
               if Match /= null and not Dups.Contains (Ref) then
                  if CSSProperty_List (Rule.Style) = CSSProperty_List (Match.Style) then
                     Dups.Insert (Ref);
                     Report.Warning (Match.Get_Location,
                                     "This rule has the same properties as rule at line "
                                     & CSS.Core.To_String (Rule.Get_Location));
                     Report.Warning (Rule.Get_Location, "Selector '"
                                     & CSS.Core.Selectors.To_String (Match.Selectors)
                                     & "' could be added "
                                     & "to rule at line "
                                     & CSS.Core.To_String (Match.Get_Location));
                  end if;
               end if;
            end;
            CSS.Core.Vectors.Next (Iter);
         end loop;
      end Process;

   begin
      Rules.Iterate (Process'Access);
   end Analyze;

end CSS.Analysis.Duplicates;
