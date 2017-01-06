-----------------------------------------------------------------------
--  json -- JSON Reader
--  Copyright (C) 2010, 2011, 2014 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Command_Line;
with Util.Serialize.IO.JSON;
with Ada.Containers;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with CSS.Parser.Parser;
with CSS.Parser.Lexer;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with CSS.Parser.Lexer_dfa;
procedure CssTools is

   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;

begin
   CSS.Parser.Lexer_dfa.aflex_debug := False;

   if Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: csstools file...");
      return;
   end if;

   for I in 1 .. Count loop
      declare
         S    : constant String := Ada.Command_Line.Argument (I);
         Res  : Integer;
      begin
         Res := CSS.Parser.Parser.Parse (S);
         Ada.Text_IO.Put_Line ("Result: " & Integer'Image (Res));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Comments: ");
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (CSS.Parser.Lexer.Current_Comment));
exception
   when E : others =>
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      raise;
end CssTools;
