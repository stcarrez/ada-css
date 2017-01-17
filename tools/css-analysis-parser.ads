-----------------------------------------------------------------------
--  css-analysis-parser -- Rule parser for CSS Analysis
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
with Ada.Strings.Unbounded;
with CSS.Analysis.Rules;
package CSS.Analysis.Parser is

   --  The parser token or expression.
   type YYstype is private;

   procedure Load (Path : in String);

private

   type YYstype is record
      Token      : Ada.Strings.Unbounded.Unbounded_String;
      Line       : Natural;
      Column     : Natural;
      Min_Repeat : Natural := 0;
      Max_Repeat : Natural := 0;
      Rule       : CSS.Analysis.Rules.Rule_Type_Access;
   end record;

   procedure Set_Ident (Into   : in out YYstype;
                        Text   : in String;
                        Line   : in Natural;
                        Column : in Natural);

   --  Create a property rule.
   procedure Create_Property (Name : in YYstype;
                              Rule : in YYstype);

   --  Create a definition rule.
   procedure Create_Definition (Name : in YYstype;
                                Rule : in YYstype);

end CSS.Analysis.Parser;
