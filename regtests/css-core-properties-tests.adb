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
with CSS.Core.Values;
package body CSS.Core.Properties.Tests is

   package Caller is new Util.Test_Caller (Test, "CSS.Core");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test CSS.Core.Properties.Append",
                       Test_Append'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test stylesheet creation.
   --  ------------------------------
   procedure Test_Append (T : in out Test) is
      Sheet : Stylesheet;
      List  : CSSProperty_List;
      Repo  : CSS.Core.Values.Repository_Type;
      N     : CSSProperty_Name := Sheet.Create_Property_Name ("border");
      V     : Value_Type := Repo.Create_Number ("23");
      P     : CSSProperty;
   begin
      Util.Tests.Assert_Equals (T, 0, List.Get_Length, "Get_Length returned invalid length");
      List.Append (N, V);
      Util.Tests.Assert_Equals (T, 1, List.Get_Length, "Get_Length returned invalid length");
      P := List.Get_Property ("border");
      Util.Tests.Assert_Equals (T, "border", P.Name.all, "Get_Property returned invalid property");
      Util.Tests.Assert_Equals (T, "23", P.Value.To_String, "Get_Property returned invalid property");
   end Test_Append;

end CSS.Core.Properties.Tests;
