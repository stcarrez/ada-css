-----------------------------------------------------------------------
--  csstools -- CSS Tools Command
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
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.Traceback.Symbolic;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;
with Util.Log.Loggers;
with Util.Files;
with CSS.Parser.Lexer;
with CSS.Parser.Lexer_dfa;
with CSS.Analysis.Parser.Lexer_dfa;
with CSS.Core;
with CSS.Core.Sheets;
with CSS.Tools.Messages;
with CSS.Core.Sets;
with CSS.Analysis.Rules;
with CSS.Analysis.Duplicates;
with CSS.Printer.Text_IO;
with CSS.Analysis.Parser;
with CSS.Analysis.Rules.Main;

procedure CssTools is

   use Ada.Strings.Unbounded;
   use Ada.Containers;
   use Ada.Directories;
   use Ada.Command_Line;

   procedure Usage;
   procedure Print_Message (Severity : in CSS.Tools.Messages.Severity_Type;
                            Loc      : in CSS.Core.Location;
                            Message  : in String);
   procedure Set_Config_Directory (Path   : in String;
                                   Silent : in Boolean := False);

   Debug       : Boolean := False;
   Verbose     : Boolean := False;
   Quiet       : Boolean := False;
   Status      : Exit_Status := Success;
   Doc         : aliased CSS.Core.Sheets.CSSStylesheet;
   Err_Handler : aliased CSS.Tools.Messages.Message_List;
   Output_Path : Unbounded_String;
   Config_Dir  : Unbounded_String;
   Output      : CSS.Printer.Text_IO.File_Type;
   Dup_Rules   : CSS.Core.Sets.Set;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("CSS Analysis tools");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("csstools [-dpqv] [-o file] [-c dir] file...");
      Ada.Text_IO.Put_Line ("  -d       Turn on debugging");
      Ada.Text_IO.Put_Line ("  -v       Verbose mode");
      Ada.Text_IO.Put_Line ("  -q       Quiet mode");
      Ada.Text_IO.Put_Line ("  -p       Pretty print CSS output");
      Ada.Text_IO.Put_Line ("  -o file  Generate the CSS output file");
      Ada.Text_IO.Put_Line ("  -c dir   Define the path for the configuration directory");
   end Usage;

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
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");

   Output.Compress := True;

   --  Parse the command line
   begin
      loop
         case Getopt ("v p d q r: c: o: ") is
            when ASCII.NUL => exit;

            when 'c' =>
               Set_Config_Directory (Parameter);

            when 'r' =>
               Config_Path := To_Unbounded_String (Parameter);

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
      end loop;

   exception
      when others =>
         Usage;
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
         --  Set_Config_Directory (Gen.Configs.CONFIG_DIR, True);
      end;
   end if;

   CSS.Parser.Lexer_dfa.aflex_debug := Debug;
   CSS.Analysis.Parser.Lexer_dfa.aflex_debug := Debug;
   if Length (Config_Path) > 0 then
      CSS.Analysis.Parser.Load (To_String (Config_Path), CSS.Analysis.Rules.Main.Rule_Repository);
   end if;
   if Length (Config_Dir) > 0 then
      CSS.Analysis.Parser.Load_All (To_String (Config_Dir) & "/rules", CSS.Analysis.Rules.Main.Rule_Repository);
   end if;

   if Length (Output_Path) > 0 then
      Ada.Text_IO.Create (Output.File, Ada.Text_IO.Out_File, To_String (Output_Path));
   end if;
   if Verbose then
      CSS.Analysis.Rules.Print (Output, CSS.Analysis.Rules.Main.Rule_Repository.all);
   end if;
   loop
      declare
         Path : constant String := Get_Argument;
      begin
         exit when Path'Length = 0;
         Doc.Set_Href (Path);
         CSS.Parser.Load (Path, Doc'Unchecked_Access, Err_Handler'Unchecked_Access);
         CSS.Analysis.Duplicates.Analyze (Doc.Rules, Err_Handler, Dup_Rules);
         CSS.Analysis.Rules.Main.Analyze (Doc, Err_Handler);
         Err_Handler.Iterate (Print_Message'Access);
         if Length (Output_Path) > 0 then
            Output.Print (Doc);
         end if;
         if Err_Handler.Get_Error_Count > 0 then
            Status := Failure;
         end if;
      end;
   end loop;
   if not Quiet then
      if Verbose then
         Ada.Text_IO.Put_Line ("Comments: ");
         Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (CSS.Parser.Lexer.Current_Comment));
      end if;
      Ada.Text_IO.Put_Line ("Errors          : " & Natural'Image (Err_Handler.Get_Error_Count));
      Ada.Text_IO.Put_Line ("Warnings        : " & Natural'Image (Err_Handler.Get_Warning_Count));
      Ada.Text_IO.Put_Line ("CSS rules       : " & Count_Type'Image (Doc.Rules.Length));
      Ada.Text_IO.Put_Line ("CSS values      : " & Natural'Image (Doc.Values.Length));
      Ada.Text_IO.Put_Line ("Duplicate rules : " & Count_Type'Image (Dup_Rules.Length));
   end if;
   Ada.Command_Line.Set_Exit_Status (Status);

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      Usage;
      Ada.Command_Line.Set_Exit_Status (2);

   when E : others =>
      Err_Handler.Iterate (Print_Message'Access);
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);

end CssTools;
