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
with CSS.Core.Sets;
with CSS.Core.Vectors;
with CSS.Core.Errors;
package CSS.Analysis.Duplicates is

   --  Analyze the CSS rules defined in the vectors to check for duplicate rules.
   --  Report a warning for each rule that appears to be a duplicate of another.
   --  Collect in <tt>Dups</tt> the set of rules which are duplicates.
   --  The initial <tt>Dups</tt> set can already contain existing rules.
   procedure Analyze (Rules  : in CSS.Core.Vectors.Vector;
                      Report : in out CSS.Core.Errors.Error_Handler'Class;
                      Dups   : in out CSS.Core.Sets.Set);

end CSS.Analysis.Duplicates;
