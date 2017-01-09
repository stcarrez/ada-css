-----------------------------------------------------------------------
--  CSS testsuite - Ada CSS Test suite
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
with CSS.Core.Tests;
with CSS.Core.Properties.Tests;
with CSS.Comments.Tests;
with CSS.Parser.Tests;
package body CSS.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      CSS.Comments.Tests.Add_Tests (Ret);
      CSS.Core.Tests.Add_Tests (Ret);
      CSS.Core.Properties.Tests.Add_Tests (Ret);
      CSS.Parser.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end CSS.Testsuite;
