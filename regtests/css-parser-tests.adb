-----------------------------------------------------------------------
--  css-parser-tests -- Unit tests for CSS parser
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
with Ada.Directories;

with Util.Measures;

with CSS.Parser.Parser;
package body CSS.Parser.Tests is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Test case name
   --  ------------------------------
   overriding
   function Name (T : in Test) return Util.Tests.Message_String is
   begin
      return Util.Tests.Format ("Test CSS parser " & To_String (T.Name));
   end Name;

   --  ------------------------------
   --  Perform the test.
   --  ------------------------------
   overriding
   procedure Run_Test (T : in out Test) is
      Res : Integer;
   begin
      Res := CSS.Parser.Parser.Parse (Ada.Strings.Unbounded.To_String (T.File));
      Util.Tests.Assert_Equals (T, 0, CSS.Parser.Parser.Error_Count,
                                "Errors reported for '" & To_String (T.Name));
   end Run_Test;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      use Ada.Directories;

      procedure Add_Parser_Tests;

      function Create_Test (Name    : in String;
                            Path    : in String) return Test_Case_Access;

      Result_Dir  : constant String := "regtests/result";
      Expect_Dir  : constant String := "regtests/expect";
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path (Result_Dir);
      Search      : Search_Type;
      Filter      : constant Filter_Type := (others => True);
      Ent         : Directory_Entry_Type;

      function Create_Test (Name    : in String;
                            Path    : in String) return Test_Case_Access is
         Tst    : Test_Case_Access;
      begin
         Tst := new Test;
         Tst.Name    := To_Unbounded_String (Name);
         Tst.File    := To_Unbounded_String (Path);
         Tst.Expect  := To_Unbounded_String (Expect_Path & Name);
         Tst.Result  := To_Unbounded_String (Result_Path & Name);
         return Tst;
      end Create_Test;

      procedure Add_Parser_Tests is
         Dir         : constant String := "regtests/files/parser";
         Path        : constant String := Util.Tests.Get_Path (Dir);
      begin
         if Kind (Path) /= Directory then
            Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
         end if;

         Start_Search (Search, Directory => Path, Pattern => "*.css", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Simple : constant String := Simple_Name (Ent);
               Name   : constant String := Base_Name (Simple);
               Tst    : Test_Case_Access;
            begin
               if Simple /= "." and then Simple /= ".."
                 and then Simple /= ".svn" and then Simple (Simple'Last) /= '~'
               then
                  Tst := Create_Test (Name, Path & "/" & Simple);
                  if Tst /= null then
                     Suite.Add_Test (Tst.all'Access);
                  end if;
               end if;
            end;
         end loop;
      end Add_Parser_Tests;

   begin
      Add_Parser_Tests;
   end Add_Tests;

end CSS.Parser.Tests;
