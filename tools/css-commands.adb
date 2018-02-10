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
package body CSS.Commands is

   --  ------------------------------
   --  Load the CSS files.
   --  ------------------------------
   procedure Load (Args    : in out Argument_List'Class;
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
