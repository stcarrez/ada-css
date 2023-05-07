-----------------------------------------------------------------------
--  css-analysis-classes -- CSS Class Analysis
--  Copyright (C) 2017, 2023 Stephane Carrez
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

with Util.Log.Loggers;
with CSS.Core.Vectors;
with CSS.Core.Styles;
package body CSS.Analysis.Classes is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("CSS.Analysis.Classes");

   --  ------------------------------
   --  Analyze the CSS selectors used by the stylesheet and collect the class
   --  information with the associated CSS rules.
   --  ------------------------------
   procedure Analyze (Sheet   : in CSS.Core.Sheets.CSSStylesheet;
                      Result  : in out Class_Maps.Map;
                      Report  : in out CSS.Core.Errors.Error_Handler'Class) is
      pragma Unreferenced (Report);
      use CSS.Core.Styles;
      use CSS.Core.Selectors;

      procedure Process (Rule_Pos : in CSS.Core.Vectors.Cursor);

      procedure Process (Rule_Pos : in CSS.Core.Vectors.Cursor) is
         procedure Update (Key   : in String;
                           Value : in out Class_Information);
         procedure Analyze_One (Sel : in CSSSelector);
         procedure Analyze_Selector (Sel : in CSSSelector);

         Rule : constant CSSStyleRule_Access := Element (Rule_Pos);

         procedure Update (Key   : in String;
                           Value : in out Class_Information) is
         begin
            Log.Info ("Adding {0} to {1}", CSS.Core.To_String (Rule.Get_Location), Key);
            Value.List.Include (CSS.Core.Vectors.Element (Rule_Pos));
            Value.Loc.Include (Rule.Get_Location);
         end Update;

         procedure Analyze_One (Sel : in CSSSelector) is
            Pos : Class_Maps.Cursor;
         begin
            if Get_Selector_Type (Sel) = SEL_CLASS then
               declare
                  Name : constant String := CSS.Core.Selectors.Get_Value (Sel);
               begin
                  Pos := Result.Find (Name);
                  if Class_Maps.Has_Element (Pos) then
                     Result.Update_Element (Pos, Update'Access);
                  else
                     declare
                        Item : Class_Information;
                     begin
                        Log.Info ("New CSS class {0}", Name);
                        Item.Name := Sel;
                        Item.List.Include (CSS.Core.Vectors.Element (Rule_Pos));
                        Item.Loc.Include (Rule.Get_Location);
                        Result.Insert (Name, Item);
                     end;
                  end if;
               end;
            end if;
         end Analyze_One;

         procedure Analyze_Selector (Sel : in CSSSelector) is
         begin
            CSS.Core.Selectors.Iterate (Sel, Analyze_One'Access);
         end Analyze_Selector;

      begin
         if Rule = null then
            return;
         end if;
         CSS.Core.Selectors.Iterate (Rule.Selectors, Analyze_Selector'Access);
      end Process;

   begin
      Sheet.Rules.Iterate (Process'Access);
   end Analyze;

end CSS.Analysis.Classes;
