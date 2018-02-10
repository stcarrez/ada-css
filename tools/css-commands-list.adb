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
package body CSS.Commands.List is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd     : in Command;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out CSS.Commands.Context_Type) is
      procedure Process (Pos : in CSS.Analysis.Classes.Cursor) is
      begin
         Ada.Text_IO.Put_Line (CSS.Analysis.Classes.Class_Maps.Key (Pos));
      end Process;
   begin
      CSS.Commands.Load (Args, Context);
      CSS.Analysis.Classes.Analyze (Context.Doc, Context.Class_Map, Context.Err_Handler);
      Context.Class_Map.Iterate (Process'Access);
   end Execute;

   --  Write the help associated with the command.
   overriding
   procedure Help (Cmd     : in Command;
                   Context : in out CSS.Commands.Context_Type) is
   begin
      null;
   end Help;

end CSS.Commands.List;
