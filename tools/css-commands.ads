-----------------------------------------------------------------------
--  css-commands -- Commands for CSS tools
--  Copyright (C) 2018, 2020, 2023 Stephane Carrez
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
with Util.Strings;
with Util.Commands.Drivers;
with Util.Commands.Parsers;
with Util.Commands.Consoles;
with Util.Commands.Consoles.Text;
with Util.Commands.Text_IO;
with CSS.Printer.Text_IO;
with CSS.Core.Sets;
with CSS.Core.Sheets;
with CSS.Tools.Messages;
with CSS.Analysis.Classes;
package CSS.Commands is

   --  The type of notice that are reported.
   type Notice_Type is (N_HELP, N_USAGE, N_INFO);

   --  The possible fields.
   type Field_Type is (F_CLASS_NAME, F_COLOR,
                       F_VALUE_1, F_VALUE_2, F_VALUE_3, F_VALUE_4, F_VALUE_LAST);

   --  Make the generic abstract console interface.
   package Consoles is
     new Util.Commands.Consoles (Field_Type  => Field_Type,
                                 Notice_Type => Notice_Type,
                                 Element_Type => Character,
                                 Input_Type   => String,
                                 To_Input     => Util.Strings.Image);

   subtype Console_Access is Consoles.Console_Access;

   function To_String (S : in String) return String is (S);

   --  And the text console to write on stdout (a Gtk console could be done someday).
   package Text_Consoles is
      new Consoles.Text (IO => Util.Commands.Text_IO, To_String => To_String);

   type Context_Type is limited record
      Doc         : aliased CSS.Core.Sheets.CSSStylesheet;
      Err_Handler : aliased CSS.Tools.Messages.Message_List;
      Output      : CSS.Printer.Text_IO.File_Type;
      Report      : CSS.Printer.Text_IO.File_Type;
      Dup_Rules   : CSS.Core.Sets.Set;
      Class_Map   : CSS.Analysis.Classes.Class_Maps.Map;
      Console     : Console_Access;
   end record;

   package Drivers is
     new Util.Commands.Drivers (Context_Type  => Context_Type,
                                Driver_Name   => "gen-commands",
                                Config_Parser => Util.Commands.Parsers.No_Parser,
                                IO            => Util.Commands.Text_IO);

   subtype Command is Drivers.Command_Type;
   subtype Command_Access is Drivers.Command_Access;
   subtype Argument_List is Util.Commands.Argument_List;

   Driver : Drivers.Driver_Type;

   --  Load the CSS files.
   procedure Load (Args    : in Argument_List'Class;
                   Context : in out Context_Type);

   --  Initialize the context.
   procedure Initialize (Context : in out Context_Type);

   --  Print csstools short usage.
   procedure Short_Help_Usage;

end CSS.Commands;
