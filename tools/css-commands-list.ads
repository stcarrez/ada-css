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
package CSS.Commands.List is

   --  ------------------------------
   --  List Command
   --  ------------------------------
   --  This command lists some information about the CSS files.
   type Command is new CSS.Commands.Command with null record;

   --  Execute the command with the arguments.
   overriding
   procedure Execute (Cmd     : in out Command;
                      Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out CSS.Commands.Context_Type);

   --  Write the help associated with the command.
   overriding
   procedure Help (Cmd     : in out Command;
                   Name    : in String;
                   Context : in out CSS.Commands.Context_Type);

end CSS.Commands.List;
