-----------------------------------------------------------------------
--  css-core-tests -- Unit tests for CSS core package
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

with Util.Test_Caller;

package body CSS.Core.Tests is

   package Caller is new Util.Test_Caller (Test, "CSS.Core");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test CSS.Core.Stylesheet",
                       Test_Create_Stylesheet'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test stylesheet creation.
   --  ------------------------------
   procedure Test_Create_Stylesheet (T : in out Test) is
   begin
      --  Util.Tests.Assert_Equals (T, "<p><b>bold</b></p>",
      --                          Wiki.Utils.To_Html ("*bold*", SYNTAX_GOOGLE),
      --                          "Bold rendering invalid");
      null;
   end Test_Create_Stylesheet;

end CSS.Core.Tests;
