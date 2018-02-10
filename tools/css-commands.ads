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
with Util.Commands.Drivers;
with CSS.Printer.Text_IO;
with CSS.Core.Sets;
with CSS.Core.Sheets;
with CSS.Tools.Messages;
with CSS.Analysis.Classes;
package CSS.Commands is

   type Context_Type is limited record
      Doc         : aliased CSS.Core.Sheets.CSSStylesheet;
      Err_Handler : aliased CSS.Tools.Messages.Message_List;
      Output      : CSS.Printer.Text_IO.File_Type;
      Report      : CSS.Printer.Text_IO.File_Type;
      Dup_Rules   : CSS.Core.Sets.Set;
      Class_Map   : CSS.Analysis.Classes.Class_Maps.Map;
   end record;

   package Drivers is
     new Util.Commands.Drivers (Context_Type => Context_Type,
                                Driver_Name  => "gen-commands");

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
