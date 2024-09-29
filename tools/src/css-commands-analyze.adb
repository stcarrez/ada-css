-----------------------------------------------------------------------
--  css-commands-analyze -- List command for CSS tools
--  Copyright (C) 2018, 2020 Stephane Carrez
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
with CSS.Analysis.Duplicates;
with CSS.Analysis.Rules.Main;
package body CSS.Commands.Analyze is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context : in out CSS.Commands.Context_Type) is
      pragma Unreferenced (Cmd, Name);
   begin
      CSS.Commands.Load (Args, Context);
      CSS.Analysis.Duplicates.Analyze (Context.Doc.Rules, Context.Err_Handler,
                                       Context.Dup_Rules);
      CSS.Analysis.Rules.Main.Analyze (Context.Doc, Context.Err_Handler);
--        Err_Handler.Iterate (Print_Message'Access);
      CSS.Analysis.Classes.Analyze (Context.Doc, Context.Class_Map,
                                    Context.Err_Handler);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Cmd     : in out Command;
                   Name    : in String;
                   Context : in out CSS.Commands.Context_Type) is
      pragma Unreferenced (Cmd, Name);

      Console : constant CSS.Commands.Console_Access := Context.Console;
   begin
      Console.Notice (N_HELP, "analyze: analyze the CSS files");
   end Help;

end CSS.Commands.Analyze;
