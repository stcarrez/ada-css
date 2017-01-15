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
package CSS.Core.Errors.Default is

   type Error_Handler is limited new CSS.Core.Errors.Error_Handler with record
      File          : Ada.Text_IO.File_Type;
      Error_Count   : Natural := 0;
      Warning_Count : Natural := 0;
   end record;

   --  Report an error message at the given CSS source position.
   overriding
   procedure Error (Handler : in out Error_Handler;
                    Loc     : in Location;
                    Message : in String);

   --  Report a warning message at the given CSS source position.
   overriding
   procedure Warning (Handler : in out Error_Handler;
                      Loc     : in Location;
                      Message : in String);

end CSS.Core.Errors.Default;
