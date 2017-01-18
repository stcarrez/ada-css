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
with Util.Log.Loggers;
with Ada.Strings.Unbounded;
with CSS.Analysis.Rules;

--  == CSS property rule definition parser ==
--  The CSS property rules are described in text files whose format follows the Mozilla
--  value definition syntax.  The documentation of such syntax is described by these
--  links:
--
--  * https://developer.mozilla.org/fr/docs/Web/CSS/Syntaxe_de_d%C3%A9finition_des_valeurs
--  * https://developer.mozilla.org/en-US/docs/Web/CSS/Value_definition_syntax
--
--  The parser reads the rule definition files and build the analyzer rules by creating
--  either property rules or named definition rules in the rule repository.
package CSS.Analysis.Parser is

   --  The parser token or expression.
   type YYstype is private;

   --  Load the rule definition file and populate the rule repository.
   procedure Load (Path : in String);

   --  Load all the rule definition files stored in the given directory.
   procedure Load_All (Path : in String);

private

   --  Logger used by the parser to report errors or debug messages.
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("CSS.Analysis.Parse");

   type YYstype is record
      Token      : Ada.Strings.Unbounded.Unbounded_String;
      Line       : Natural;
      Column     : Natural;
      Min_Repeat : Natural := 0;
      Max_Repeat : Natural := 0;
      Rule       : CSS.Analysis.Rules.Rule_Type_Access;
   end record;

   --  Get the value token as an integer.
   function Get_Value (Token : in YYstype) return Natural;

   --  Set the token to represent an identifier at the given source position.
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

   --  Create a type or a reference to a definition rule.
   procedure Create_Type_Or_Reference (Into : out YYstype;
                                       Name : in YYstype);

   --  Create an identifier rule.
   procedure Create_Identifier (Into : out YYstype;
                                Name : in YYstype);

   --  Append or make a group of rules.
   procedure Append_Group (Into   : out YYstype;
                           Group  : in YYstype;
                           Item   : in YYstype;
                           Kind   : in Rules.Group_Type);

   --  Report an error message while parsing the rule definition file.
   procedure Error (Line    : in Natural;
                    Column  : in Natural;
                    Message : in String);

end CSS.Analysis.Parser;
