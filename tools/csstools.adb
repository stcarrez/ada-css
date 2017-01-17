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
with Util.Streams.Texts;
with Util.Streams.Buffered;
with CSS.Parser.Parser;
with CSS.Parser.Lexer;
with Ada.Exceptions;
with Ada.Containers.Ordered_Sets;
with CSS.Parser.Lexer_dfa;
with CSS.Analysis.Parser.Lexer_dfa;
with CSS.Core;
with CSS.Core.Refs;
with CSS.Core.Sheets;
with CSS.Core.Vectors;
with CSS.Core.Styles;
with CSS.Core.Selectors;
with CSS.Core.Properties;
with CSS.Core.Errors.Default;
with CSS.Tools.Messages;
with CSS.Core.Compare;
with CSS.Core.Sets;
with CSS.Analysis.Duplicates;
with CSS.Printer.Text_IO;
with CSS.Analysis.Parser;
with System;
procedure CssTools is

   use Util.Streams.Buffered;
   use Ada.Strings.Unbounded;
   use Ada.Containers;

   Debug       : Boolean := False;
   Verbose     : Boolean := False;
   Quiet       : Boolean := False;
   Count       : constant Natural := Ada.Command_Line.Argument_Count;
   Doc         : aliased CSS.Core.Sheets.CSSStylesheet;
   Err_Handler : aliased CSS.Tools.Messages.Message_List;
   Output_Path : Unbounded_String;
   Output      : CSS.Printer.Text_IO.Context_Type;
   Dup_Count   : Natural := 0;
   Dup_Rules   : CSS.Core.Sets.Set;

   procedure Print (Rule : in CSS.Core.Styles.CSSStyleRule_Access) is
      procedure Print (Prop : in CSS.Core.Properties.CSSProperty) is
      begin
         Ada.Text_IO.Put ("   ");
	 Ada.Text_IO.Put (Prop.Name.all);
	 Ada.Text_IO.Put (": ");
	 Ada.Text_IO.Put (Prop.Value.To_String);
	 Ada.Text_IO.Put_Line (";");
      end Print;
   begin
      Ada.Text_IO.Put_Line (CSS.Core.Selectors.To_String (Rule.Selectors) & " {");
      Rule.Style.Iterate (Print'Access);
      Ada.Text_IO.Put_Line ("}");
   end Print;

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
   loop
      case Getopt ("v p d q c: o: ") is
         when ASCII.NUL => exit;

         when 'c' =>
            --  Set_Config_Directory (Parameter);
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

   CSS.Parser.Lexer_dfa.aflex_debug := Debug;
   CSS.Analysis.Parser.Lexer_dfa.aflex_debug := Debug;
   if Length (Config_Path) > 0 then
      CSS.Analysis.Parser.Load (To_String (Config_Path));
   end if;

   if Length (Output_Path) > 0 then
      Ada.Text_IO.Create (Output.File, Ada.Text_IO.Out_File, To_String (Output_Path));
   end if;
   loop
      declare
         Path : constant String := Get_Argument;
      begin
         exit when Path'Length = 0;
         Doc.Set_Href (Path);
         CSS.Parser.Load (Path, Doc'Unchecked_Access, Err_Handler'Unchecked_Access);
         CSS.Analysis.Duplicates.Analyze (Doc.Rules, Err_Handler, Dup_Rules);
         Err_Handler.Iterate (Print_Message'Access);
         if Length (Output_Path) > 0 then
            Output.Print (Doc);
         end if;
      end;
   end loop;
   if not Quiet then
      Ada.Text_IO.Put_Line ("Comments: ");
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (CSS.Parser.Lexer.Current_Comment));

      Ada.Text_IO.Put_Line ("Errors          : " & Natural'Image (Err_Handler.Get_Error_Count));
      Ada.Text_IO.Put_Line ("Warnings        : " & Natural'Image (Err_Handler.Get_Warning_Count));
      Ada.Text_IO.Put_Line ("CSS rules       : " & Count_Type'Image (Doc.Rules.Length));
      Ada.Text_IO.Put_Line ("CSS values      : " & Natural'Image (Doc.Values.Length));
      Ada.Text_IO.Put_Line ("Duplicate rules : " & Count_Type'Image (Dup_Rules.Length));
   end if;

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      --  Gen.Commands.Short_Help_Usage;
      Ada.Command_Line.Set_Exit_Status (2);

   when E : others =>
      Err_Handler.Iterate (Print_Message'Access);
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
      raise;
end CssTools;
