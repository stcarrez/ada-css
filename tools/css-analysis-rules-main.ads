-----------------------------------------------------------------------
--  css-analysis-rules-main -- Rule repository
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
package CSS.Analysis.Rules.Main is

   --  Analyze the CSS rules with their properties against the repository
   --  definition and report any warning or error.
   procedure Analyze (Sheet      : in CSS.Core.Sheets.CSSStylesheet;
                      Report     : in out CSS.Core.Errors.Error_Handler'Class);

   --  Get the rule repository instance.
   function Rule_Repository return access Repository_Type;

end CSS.Analysis.Rules.Main;
