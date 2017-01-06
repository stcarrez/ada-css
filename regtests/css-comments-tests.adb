-----------------------------------------------------------------------
--  css-comments-tests -- Unit tests for CSS comments package
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

package body CSS.Comments.Tests is

   package Caller is new Util.Test_Caller (Test, "CSS.Comments");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test CSS.Comments.Get_Text (empty)",
                       Test_Empty'Access);
      Caller.Add_Test (Suite, "Test CSS.Comments.Insert",
                       Test_Insert'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test stylesheet creation.
   --  ------------------------------
   procedure Test_Empty (T : in out Test) is
      E : Comment_Type;
   begin
      Util.Tests.Assert_Equals (T, 0, Get_Line_Number (E), "Get_Line_Number must return 0");
      Util.Tests.Assert_Equals (T, 0, Get_Line_Count (E), "Get_Line_Count must return 0");
      Util.Tests.Assert_Equals (T, "", Get_Text (E), "Get_Text must return empty string");
      Util.Tests.Assert_Equals (T, "", Get_Text (E, 1), "Get_Text must return empty string");
      Util.Tests.Assert_Equals (T, "", Get_Text (E, 2), "Get_Text must return empty string");
   end Test_Empty;

   --  Test inserting a comment in the comment list.
   procedure Test_Insert (T : in out Test) is
      C1, C2, C3   : Comment_Type;
      List         : CSSComment_List;
   begin
      List.Insert ("Comment one", 1, C1);
      List.Insert ("Comment two" & ASCII.LF & "second line", 2, C2);
      List.Insert ("Comment three" & ASCII.LF & ASCII.LF & "third line", 4, C3);

      --  Check C1
      Util.Tests.Assert_Equals (T, 1, Get_Line_Number (C1), "Get_Line_Number must return 1");
      Util.Tests.Assert_Equals (T, 1, Get_Line_Count (C1), "Get_Line_Count must return 1");
      Util.Tests.Assert_Equals (T, "Comment one", Get_Text (C1), "Get_Text is invalid on C1");
      Util.Tests.Assert_Equals (T, "Comment one", Get_Text (C1, 1), "Get_Text is invalid on C1");

      Util.Tests.Assert_Equals (T, 2, Get_Line_Number (C2), "Get_Line_Number must return 2");
      Util.Tests.Assert_Equals (T, 2, Get_Line_Count (C2), "Get_Line_Count must return 2");
      Util.Tests.Assert_Equals (T, "Comment two" & ASCII.LF & "second line",
                                Get_Text (C2), "Get_Text is invalid on C2");
      Util.Tests.Assert_Equals (T, "Comment two",
                                Get_Text (C2, 1), "Get_Text (1) is invalid on C2");
      Util.Tests.Assert_Equals (T, "second line",
                                Get_Text (C2, 2), "Get_Text (2) is invalid on C2");

      Util.Tests.Assert_Equals (T, 4, Get_Line_Number (C3), "Get_Line_Number must return 4");
      Util.Tests.Assert_Equals (T, 3, Get_Line_Count (C3), "Get_Line_Count must return 3");
      Util.Tests.Assert_Equals (T, "Comment three" & ASCII.LF & ASCII.LF & "third line",
                                Get_Text (C3), "Get_Text is invalid on C3");
      Util.Tests.Assert_Equals (T, "Comment three",
                                Get_Text (C3, 1), "Get_Text (1) is invalid on C3");
      Util.Tests.Assert_Equals (T, "",
                                Get_Text (C3, 2), "Get_Text (2) is invalid on C3");
      Util.Tests.Assert_Equals (T, "third line",
                                Get_Text (C3, 3), "Get_Text (3) is invalid on C3");
   end Test_Insert;

end CSS.Comments.Tests;
