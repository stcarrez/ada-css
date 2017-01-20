-----------------------------------------------------------------------
--  css-printer -- CSS printer tools
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
package body CSS.Printer.Text_IO is

   overriding
   procedure New_Line (Stream : in out File_Type) is
   begin
      Ada.Text_IO.New_Line (Stream.File);
   end New_Line;

   overriding
   procedure Print (Stream  : in out File_Type;
                    Content : in String) is
   begin
      Ada.Text_IO.Put (Stream.File, Content);
   end Print;

   overriding
   procedure Print (Stream  : in out File_Type;
                    Content : in Character) is
   begin
      Ada.Text_IO.Put (Stream.File, Content);
   end Print;

end CSS.Printer.Text_IO;
