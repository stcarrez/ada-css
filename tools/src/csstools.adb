-----------------------------------------------------------------------
--  csstools -- CSS Tools Command
--  Copyright (C) 2017, 2018, 2020, 2023 Stephane Carrez
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
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.Traceback.Symbolic;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Util.Log.Loggers;
with Util.Files;
with Util.Commands;
with CSS.Parser.Lexer_DFA;
with CSS.Analysis.Parser.Lexer_dfa;
with CSS.Core;
with CSS.Tools.Messages;
with CSS.Printer.Text_IO;
with CSS.Analysis.Parser;
with CSS.Analysis.Rules.Main;
with CSS.Tools.Configs;
with CSS.Commands;
procedure CssTools is

   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use Ada.Command_Line;

   procedure Print_Message (Severity : in CSS.Tools.Messages.Severity_Type;
                            Loc      : in CSS.Core.Location;
                            Message  : in String);
   procedure Set_Config_Directory (Path   : in String;
                                   Silent : in Boolean := False);

   Debug       : Boolean := False;
   Verbose     : Boolean := False;
   Quiet       : Boolean := False;
   Status      : Exit_Status := Success;
   Output_Path : Unbounded_String;
   Report_Path : Unbounded_String;
   Config_Dir  : Unbounded_String;
   Output      : CSS.Printer.Text_IO.File_Type;
   Report      : CSS.Printer.Text_IO.File_Type;
   Context     : CSS.Commands.Context_Type;
   First       : Natural := 0;
   Console     : aliased CSS.Commands.Text_Consoles.Console_Type;
   All_Args    : Util.Commands.Default_Argument_List (0);

   --  ------------------------------
   --  Verify and set the configuration path
   --  ------------------------------
   procedure Set_Config_Directory (Path   : in String;
                                   Silent : in Boolean := False) is
      Log_Path : constant String := Ada.Directories.Compose (Path, "csstools.properties");
   begin
      --  Ignore if the config directory was already set.
      if Length (Config_Dir) > 0 then
         return;
      end if;

      --  Check that we can read some configuration file.
      if not Ada.Directories.Exists (Log_Path) then
         if not Silent then
            Ada.Text_IO.Put_Line ("Invalid config directory: " & Path);
            Status := Failure;
         end if;
         return;
      end if;

      --  Configure the logs
      Util.Log.Loggers.Initialize (Log_Path);
      Config_Dir := To_Unbounded_String (Path);
   end Set_Config_Directory;

   procedure Print_Message (Severity : in CSS.Tools.Messages.Severity_Type;
                            Loc      : in CSS.Core.Location;
                            Message  : in String) is
      use CSS.Tools.Messages;
   begin
      Ada.Text_IO.Put (CSS.Core.To_String (Loc));
      Ada.Text_IO.Put (":");
      if Severity = MSG_ERROR then
         Ada.Text_IO.Put ("error: ");
      elsif Severity = MSG_WARNING then
         Ada.Text_IO.Put ("warning: ");
      else
         Ada.Text_IO.Put ("info: ");
      end if;
      Ada.Text_IO.Put_Line (Message);
   end Print_Message;

   Config_Path : Ada.Strings.Unbounded.Unbounded_String;
begin
   CSS.Commands.Initialize (Context);
   Context.Console := Console'Unchecked_Access;
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");

   Output.Compress     := True;
   Report.Compress     := False;
   Report.Indent_Level := 4;
   Report.Full_Semi    := True;

   --  Parse the command line
   begin
      loop
         case Getopt ("* v p d q r: R: c: o: ") is
            when ASCII.NUL => exit;

            when 'c' =>
               Set_Config_Directory (Parameter);

            when 'R' =>
               Config_Path := To_Unbounded_String (Parameter);

            when 'r' =>
               Report_Path := To_Unbounded_String (Parameter);

            when 'o' =>
               Output_Path := To_Unbounded_String (Parameter);

            when 'd' =>
               Debug := True;

            when 'p' =>
               Output.Indent_Level := 4;
               Output.Full_Semi    := True;
               Output.Compress     := False;

            when 'v' =>
               Verbose := True;

            when 'q' =>
               Quiet := True;

            when '*' =>
               exit;

            when others =>
               null;
         end case;
         First := First + 1;
      end loop;

   exception
      when others =>
         CSS.Commands.Driver.Usage (All_Args, Context);
         Ada.Command_Line.Set_Exit_Status (2);
         return;

   end;

   if Length (Config_Dir) = 0 then
      declare
         Name : constant String := Ada.Command_Line.Command_Name;
         Path : constant String := Ada.Directories.Containing_Directory (Name);
         Dir  : constant String := Ada.Directories.Containing_Directory (Path);
      begin
         Set_Config_Directory (Compose (Dir, "config"), True);
         Set_Config_Directory (Util.Files.Compose (Dir, "share/csstools"), True);
         Set_Config_Directory (CSS.Tools.Configs.CONFIG_DIR, True);
      end;
   end if;

   if Length (Config_Path) > 0 then
      CSS.Analysis.Parser.Load (To_String (Config_Path), CSS.Analysis.Rules.Main.Rule_Repository);
   end if;
   if Length (Config_Dir) > 0 then
      CSS.Analysis.Parser.Load_All (To_String (Config_Dir) & "/rules",
                                    CSS.Analysis.Rules.Main.Rule_Repository);
   end if;
   CSS.Parser.Lexer_DFA.aflex_debug := Debug;
   CSS.Analysis.Parser.Lexer_dfa.aflex_debug := Debug;

   if Length (Output_Path) > 0 then
      Ada.Text_IO.Create (Output.File, Ada.Text_IO.Out_File, To_String (Output_Path));
   end if;
   if Length (Report_Path) > 0 then
      Ada.Text_IO.Create (Report.File, Ada.Text_IO.Out_File, To_String (Report_Path));
   end if;
   if Verbose and then Length (Report_Path) > 0 then
      CSS.Analysis.Rules.Print (Report, CSS.Analysis.Rules.Main.Rule_Repository.all);
   end if;
   if First >= Ada.Command_Line.Argument_Count then
      Ada.Text_IO.Put_Line ("Missing command name to execute.");
      CSS.Commands.Driver.Usage (All_Args, Context);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   declare
      Cmd_Name : constant String := Full_Switch;
      Args     : Util.Commands.Default_Argument_List (First + 1);
   begin
      CSS.Commands.Driver.Execute (Cmd_Name, Args, Context);
   end;
   if Context.Err_Handler.Get_Error_Count > 0 then
      Status := Failure;
   end if;
   Context.Err_Handler.Iterate (Print_Message'Access);
   Ada.Command_Line.Set_Exit_Status (Status);

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      CSS.Commands.Driver.Usage (All_Args, Context);
      Ada.Command_Line.Set_Exit_Status (2);

   when E : others =>
      Context.Err_Handler.Iterate (Print_Message'Access);
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);

end CssTools;
