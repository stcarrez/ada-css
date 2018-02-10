-----------------------------------------------------------------------
--  css-commands-list -- List command for CSS tools
--  Copyright (C) 2018 Stephane Carrez
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
with CSS.Core.Values;
package body CSS.Commands.List is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd     : in Command;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out CSS.Commands.Context_Type) is
      pragma Unreferenced (Cmd);
      use type CSS.Core.Values.Value_Kind;
      procedure Process (Pos : in CSS.Analysis.Classes.Cursor);
      procedure Process_Value (Value : in CSS.Core.Values.Value_Type);

      Console : constant CSS.Commands.Console_Access := Context.Console;

      procedure Process (Pos : in CSS.Analysis.Classes.Cursor) is
      begin
         Ada.Text_IO.Put_Line (CSS.Analysis.Classes.Class_Maps.Key (Pos));
      end Process;

      procedure Process_Value (Value : in CSS.Core.Values.Value_Type) is
      begin
         if CSS.Core.Values.Get_Type (Value) = CSS.Core.Values.VALUE_COLOR then
            Ada.Text_IO.Put_Line (CSS.Core.Values.To_String (Value));
         end if;
      end Process_Value;
   begin
      CSS.Commands.Load (Args, Context);
      if Name = "list" then
         CSS.Analysis.Classes.Analyze (Context.Doc, Context.Class_Map, Context.Err_Handler);
         Context.Class_Map.Iterate (Process'Access);
      end if;
      if Name = "list-colors" then
         Context.Doc.Values.Iterate (Process_Value'Access);
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Cmd     : in Command;
                   Context : in out CSS.Commands.Context_Type) is
      pragma Unreferenced (Cmd);
      Console : constant CSS.Commands.Console_Access := Context.Console;
   begin
      Console.Notice (N_HELP, "list: list information about the CSS files");
   end Help;

end CSS.Commands.List;
