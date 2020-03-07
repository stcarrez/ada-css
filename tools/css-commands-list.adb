-----------------------------------------------------------------------
--  css-commands-list -- List command for CSS tools
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
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with CSS.Core.Values;
with CSS.Core.Properties;
with CSS.Analysis.Rules.Main;
package body CSS.Commands.List is

   --  The Value_Sets package represents a set of values all of
   --  them being strictly different.
   package Value_Sets is
      new Ada.Containers.Ordered_Sets (Element_Type => CSS.Core.Values.Value_Type,
                                       "<"          => CSS.Core.Values."<",
                                       "="          => CSS.Core.Values.Compare);

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd     : in out Command;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out CSS.Commands.Context_Type) is
      pragma Unreferenced (Cmd);
      use type CSS.Core.Values.Value_Kind;
      procedure Process (Pos : in CSS.Analysis.Classes.Cursor);
      procedure Process_Value (Value : in CSS.Core.Values.Value_Type);

      Console : constant CSS.Commands.Console_Access := Context.Console;
      Expect  : CSS.Core.Values.Value_Kind;
      Column  : Field_Type := F_VALUE_1;
      Rule    : CSS.Analysis.Rules.Rule_Type_Access;
      Loc     : CSS.Core.Location;
      Values  : Value_Sets.Set;

      procedure Process (Pos : in CSS.Analysis.Classes.Cursor) is
      begin
         Ada.Text_IO.Put_Line (CSS.Analysis.Classes.Class_Maps.Key (Pos));
      end Process;

      procedure Process_Value (Value : in CSS.Core.Values.Value_Type) is
      begin
         if Column < F_VALUE_1 or Column > F_VALUE_4 then
            if Column > F_VALUE_4 then
               Console.End_Row;
            end if;
            Console.Start_Row;
            Column := F_VALUE_1;
         end if;
         Console.Print_Field (Column, CSS.Core.Values.To_String (Value));
         Column := Field_Type'Succ (Column);
      end Process_Value;

      procedure Report_Value (Value : in CSS.Core.Values.Value_Type) is
      begin
         if Column < F_VALUE_1 or Column > F_VALUE_4 then
            if Column > F_VALUE_4 then
               Console.End_Row;
            end if;
            Console.Start_Row;
            Column := F_VALUE_1;
         end if;
         Console.Print_Field (Column, CSS.Core.Values.To_String (Value));
         Column := Field_Type'Succ (Column);
      end Report_Value;

      procedure Process_Search (Prop : in CSS.Core.Properties.CSSProperty;
                                Match : in CSS.Analysis.Rules.Match_Result) is
      begin
         for M of Match.List loop
            if CSS.Analysis.Rules.Is_Rule (M.Rule, Rule) then
               for I in M.First .. M.Last loop
                  Values.Include (CSS.Core.Values.Get_Value (Prop.Value, I));
               end loop;
            end if;
         end loop;
      end Process_Search;

   begin
      CSS.Commands.Load (Args, Context);
      if Name = "list" then
         CSS.Analysis.Classes.Analyze (Context.Doc, Context.Class_Map, Context.Err_Handler);
         Context.Class_Map.Iterate (Process'Access);
      end if;
      if Name = "list-colors" then
         Values.Clear;
         Rule := CSS.Analysis.Rules.Main.Rule_Repository.Create_Definition ("<color>", Loc);
         Expect := CSS.Core.Values.VALUE_COLOR;
         Console.Start_Title;
         Console.Print_Title (F_VALUE_1, "", 20);
         Console.Print_Title (F_VALUE_2, "", 20);
         Console.Print_Title (F_VALUE_3, "", 20);
         Console.Print_Title (F_VALUE_4, "", 20);
         Console.End_Title;
         CSS.Analysis.Rules.Main.Rule_Repository.Search (Context.Doc, Rule, Process_Search'Access);

         for V of Values loop
            Report_Value (V);
         end loop;
      end if;
      if Name = "list-length" then
         Values.Clear;
         Rule := CSS.Analysis.Rules.Main.Rule_Repository.Create_Definition ("<length>", Loc);
         Expect := CSS.Core.Values.VALUE_COLOR;
         Console.Start_Title;
         Console.Print_Title (F_VALUE_1, "", 20);
         Console.Print_Title (F_VALUE_2, "", 20);
         Console.Print_Title (F_VALUE_3, "", 20);
         Console.Print_Title (F_VALUE_4, "", 20);
         Console.End_Title;
         CSS.Analysis.Rules.Main.Rule_Repository.Search (Context.Doc, Rule, Process_Search'Access);

         for V of Values loop
            Report_Value (V);
         end loop;
      end if;
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
      Console.Notice (N_HELP, "list: list information about the CSS files");
   end Help;

end CSS.Commands.List;
