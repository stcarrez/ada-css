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
with CSS.Analysis.Parser.Parser;
with Ada.Text_IO;
package body CSS.Analysis.Parser is

   procedure Load (Path : in String) is
      Res : Integer;
   begin
      Res := CSS.Analysis.Parser.Parser.Parse (Path);
      Ada.Text_IO.Put_Line ("Loaded " & Path & ": " & Integer'Image (Res));
   end Load;

   procedure Set_Ident (Into   : in out YYstype;
                        Text   : in String;
                        Line   : in Natural;
                        Column : in Natural) is
   begin
      Into.Token  := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Into.Line   := Line;
      Into.Column := Column;
   end Set_Ident;

   --  Create a property rule.
   procedure Create_Property (Name : in YYstype;
                              Rule : in YYstype) is
   begin
      null;
   end Create_Property;

   --  Create a definition rule.
   procedure Create_Definition (Name : in YYstype;
                                Rule : in YYstype) is
   begin
      null;
   end Create_Definition;

end CSS.Analysis.Parser;
