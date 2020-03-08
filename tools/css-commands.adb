-----------------------------------------------------------------------
--  css-commands -- Commands for CSS tools
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
with Ada.Command_Line;
with CSS.Parser;
with CSS.Tools.Configs;
with CSS.Commands.Analyze;
with CSS.Commands.List;
package body CSS.Commands is

   Analyze_Command : aliased CSS.Commands.Analyze.Command;
   List_Command    : aliased CSS.Commands.List.Command;
   List_Colors_Command    : aliased CSS.Commands.List.Command;
   List_Length_Command    : aliased CSS.Commands.List.Command;
   Help_Command    : aliased CSS.Commands.Drivers.Help_Command_Type;

   --  ------------------------------
   --  Load the CSS files.
   --  ------------------------------
   procedure Load (Args    : in Argument_List'Class;
                   Context : in out Context_Type) is
   begin
      for I in 1 .. Args.Get_Count loop
         declare
            Path : constant String := Args.Get_Argument (I);
         begin
            exit when Path'Length = 0;
            Context.Doc.Set_Href (Path);
            CSS.Parser.Load (Path, Context.Doc'Unchecked_Access, Context.Err_Handler'Unchecked_Access);
         end;
      end loop;
   end Load;

   --  ------------------------------
   --  Initialize the context.
   --  ------------------------------
   procedure Initialize (Context : in out Context_Type) is
   begin
      Driver.Set_Description (CSS.Tools.Configs.RELEASE);
      Driver.Set_Usage ("[-dpqv] [-o file] [-r file] [-c dir] command file..." & ASCII.LF &
                          "  -d       Turn on parser debugging" & ASCII.LF &
                          "  -v       Verbose mode" & ASCII.LF &
                          "  -q       Quiet mode" & ASCII.LF &
                          "  -p       Pretty print CSS output" & ASCII.LF &
                          "  -o file  Generate the CSS output file" & ASCII.LF &
                          "  -r file  Generate a report about the CSS file" & ASCII.LF &
                          "  -c dir   Define the path for the configuration directory");
      Driver.Add_Command ("help", Help_Command'Access);
      Driver.Add_Command ("analyze", Analyze_Command'Access);
      Driver.Add_Command ("list", List_Command'Access);
      Driver.Add_Command ("list-colors", List_Colors_Command'Access);
      Driver.Add_Command ("list-length", List_Length_Command'Access);
   end Initialize;

   --  ------------------------------
   --  Print csstools short usage.
   --  ------------------------------
   procedure Short_Help_Usage is
      use Ada.Text_IO;
   begin
      New_Line;
      Put ("Type '");
      Put (Ada.Command_Line.Command_Name);
      Put_Line (" help' for the list of commands.");
   end Short_Help_Usage;

end CSS.Commands;
