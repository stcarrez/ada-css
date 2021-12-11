-----------------------------------------------------------------------
--  css-analysis-rules-tests -- Unit tests for CSS rule analyzer
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

with CSS.Core.Sheets;
with CSS.Core.Errors.Default;
with CSS.Parser;
with CSS.Analysis.Parser;
package body CSS.Analysis.Rules.Tests is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Test case name
   --  ------------------------------
   overriding
   function Name (T : in Test) return Util.Tests.Message_String is
   begin
      return Util.Tests.Format ("Test CSS analysis " & To_String (T.Name));
   end Name;

   --  ------------------------------
   --  Perform the test.
   --  ------------------------------
   overriding
   procedure Run_Test (T : in out Test) is
      Rules_Path : constant String := Util.Tests.Get_Path ("config/rules");
      Name   : constant String := Ada.Strings.Unbounded.To_String (T.Name);
      Path   : constant String := Ada.Strings.Unbounded.To_String (T.File);
      Doc    : aliased CSS.Core.Sheets.CSSStylesheet;
      Errors : aliased CSS.Core.Errors.Default.Error_Handler;
      Repo   : aliased Repository_Type;
   begin
      CSS.Analysis.Parser.Load_All (Rules_Path, Repo'Unchecked_Access);
      declare
         Time   : Util.Measures.Stamp;
      begin
         Doc.Set_Href (Name);
         Ada.Text_IO.Create (Errors.File, Ada.Text_IO.Out_File, To_String (T.Result));
         CSS.Parser.Load (Path, Doc'Unchecked_Access, Errors'Unchecked_Access);
         CSS.Analysis.Rules.Analyze (Repo, Doc, Errors);
         Util.Measures.Report (Time, "Parse and analyse " & Name);
      end;
      Ada.Text_IO.Close (Errors.File);
      if T.Has_Error then
         T.Assert (Errors.Error_Count > 0, "No error reported for '" & Name & "'");
      else
         Util.Tests.Assert_Equals (T, 0, Errors.Error_Count,
                                   "Errors reported for '" & Name & "'");
      end if;
      Util.Tests.Assert_Equal_Files (T, Ada.Strings.Unbounded.To_String (T.Expect),
                                     To_String (T.Result), "Analysis of " & Name & " failed");

   exception
      when others =>
         if Ada.Text_IO.Is_Open (Errors.File) then
            Ada.Text_IO.Close (Errors.File);
         end if;
         raise;

   end Run_Test;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      use Ada.Directories;

      procedure Add_Parser_Tests (Dir       : in String;
                                  Has_Error : in Boolean);

      function Create_Test (Name    : in String;
                            Path    : in String) return Test_Case_Access;

      Expect_Dir  : constant String := "regtests/expect/rules/";
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path ("rules");
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

      procedure Add_Parser_Tests (Dir       : in String;
                                  Has_Error : in Boolean) is
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
                     Ada.Directories.Create_Path (Result_Path);
                     Tst.Has_Error := Has_Error;
                     Suite.Add_Test (Tst.all'Access);
                  end if;
               end if;
            end;
         end loop;
      end Add_Parser_Tests;

   begin
      Add_Parser_Tests ("regtests/files/rules", False);
   end Add_Tests;

end CSS.Analysis.Rules.Tests;
