-----------------------------------------------------------------------
--  css -- Ada CSS Library
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
with Ada.Text_IO;
with CSS.Parser.Parser;
package body CSS.Parser is

   procedure Load (Path  : in String;
                   Sheet : in CSS.Core.Stylesheet_Access) is
      Res : Integer := CSS.Parser.Parser.Parse (Path);
   begin
      null;
   end Load;

   procedure Error (Line    : in Natural;
                    Message : in String) is
      L : constant String := Natural'Image (Line);
   begin
      Ada.Text_IO.Put_Line (L (L'First + 1 .. L'Last) & ": " & Message);
   end Error;

end CSS.Parser;
