-----------------------------------------------------------------------
--  css-comments -- CSS comments recording
--  Copyright (C) 2017, 2023 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;

package body CSS.Comments is

   function Get_Line_Count (Text : in String) return Positive;

   --  ------------------------------
   --  Get the line number where the comment was written.
   --  ------------------------------
   function Get_Line_Number (Comment : in Comment_Type) return Natural is
   begin
      return Comment.Object.Line;
   end Get_Line_Number;

   --  ------------------------------
   --  Get the number of lines that the comment span.
   --  ------------------------------
   function Get_Line_Count (Comment : in Comment_Type) return Natural is
   begin
      return Comment.Object.Line_Count;
   end Get_Line_Count;

   --  ------------------------------
   --  Get the full or stripped comment text.
   --  ------------------------------
   function Get_Text (Comment : in Comment_Type;
                      Strip   : in Boolean := False) return String is
      pragma Unreferenced (Strip);
   begin
      return Comment.Object.Text;
   end Get_Text;

   --  ------------------------------
   --  Get the given line of the comment text.
   --  ------------------------------
   function Get_Text (Comment : in Comment_Type;
                      Line    : in Positive;
                      Strip   : in Boolean := False) return String is
      Start, Finish : Positive;
   begin
      if Line > Comment.Object.Line_Count then
         return "";
      else
         Start  := Comment.Object.Lines (Line).Start;
         Finish := Comment.Object.Lines (Line).Finish;
         if Strip then
            while Start <= Finish loop
               exit when Comment.Object.Text (Start) /= ' '
                 and then Comment.Object.Text (Start) /= ASCII.HT;
               Start := Start + 1;
            end loop;
            while Start <= Finish loop
               exit when Comment.Object.Text (Finish) /= ' '
                 and then Comment.Object.Text (Finish) /= ASCII.HT;
               Finish := Finish - 1;
            end loop;
         end if;
         return Comment.Object.Text (Start .. Finish);
      end if;
   end Get_Text;

   function Get_Line_Count (Text : in String) return Positive is
      Result : Positive := 1;
   begin
      for I in Text'Range loop
         if Text (I) = Ada.Characters.Latin_1.LF then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Line_Count;

   --  ------------------------------
   --  Insert in the comment list the comment described by <tt>Text</tt>
   --  and associated with source line number <tt>Line</tt>.  After insertion
   --  the comment reference is returned to identify the new comment.
   --  ------------------------------
   procedure Insert (List  : in out CSSComment_List;
                     Text  : in String;
                     Line  : in Natural;
                     Index : out Comment_Type) is
      N : constant Positive := Get_Line_Count (Text);
      C : constant Comment_Access := new Comment '(Len  => Text'Length, Line_Count => N,
                                          Text => Text, Line => Line, Next => List.Chain,
                                          Lines => (others => (1, 0)));
      First : Natural := 1;
      Last  : constant Natural := Text'Length;
      Pos   : Positive := 1;
   begin
      Index.Object := C.all'Access;
      List.Chain   := C;
      while First <= Last loop
         if Text (First) = Ada.Characters.Latin_1.LF then
            C.Lines (Pos).Finish := First - 1;
            Pos := Pos + 1;
            C.Lines (Pos).Start := First + 1;
         end if;
         First := First + 1;
      end loop;
      C.Lines (Pos).Finish := Last;
   end Insert;

   --  ------------------------------
   --  Release the memory used by the comment list.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out CSSComment_List) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Comment, Comment_Access);

      Current : Comment_Access := List.Chain;
   begin
      while Current /= null loop
         declare
            Next : constant Comment_Access := Current.Next;
         begin
            Free (Current);
            Current := Next;
         end;
      end loop;
      List.Chain := null;
   end Finalize;

end CSS.Comments;
