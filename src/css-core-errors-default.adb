-----------------------------------------------------------------------
--  css-core-errors-default -- Error handler interface
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
with Ada.Text_IO;
package body CSS.Core.Errors.Default is

   --  ------------------------------
   --  Report an error message at the given CSS source position.
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Error_Handler;
                    Loc     : in Location;
                    Message : in String) is
      L : constant String := CSS.Core.To_String (Loc);
   begin
      Handler.Error_Count := Handler.Error_Count + 1;
      if Ada.Text_IO.Is_Open (Handler.File) then
         Ada.Text_IO.Put_Line (Handler.File, L & ": error: " & Message);
      else
         Ada.Text_IO.Put_Line (L & ": error: " & Message);
      end if;
   end Error;

   --  ------------------------------
   --  Report a warning message at the given CSS source position.
   --  ------------------------------
   overriding
   procedure Warning (Handler : in out Error_Handler;
                      Loc     : in Location;
                      Message : in String) is
      L : constant String := CSS.Core.To_String (Loc);
   begin
      Handler.Warning_Count := Handler.Warning_Count + 1;
      if Ada.Text_IO.Is_Open (Handler.File) then
         Ada.Text_IO.Put_Line (Handler.File, L & ": error: " & Message);
      else
         Ada.Text_IO.Put_Line (L & ": warning: " & Message);
      end if;
   end Warning;

end CSS.Core.Errors.Default;
