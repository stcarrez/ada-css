-----------------------------------------------------------------------
--  css-comments -- CSS comments recording
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
private with Ada.Finalization;

--  == CSS Comments ==
--  The CSS comments can be recorded during the parsing of CSS content.
--  When a comment is recorded, a comment reference represented by the
--  <tt>Comment_Type</tt> type is returned.  The CSS comment can be a multi-line
--  comment and the package provides several operations to either retrieve
--  the full comment or a specific line.
package CSS.Comments is

   type Comment_Type is private;

   NULL_COMMENT : constant Comment_Type;

   --  Get the line number where the comment was written.
   function Get_Line_Number (Comment : in Comment_Type) return Natural;

   --  Get the number of lines that the comment span.
   function Get_Line_Count (Comment : in Comment_Type) return Natural;

   --  Get the full or stripped comment text.
   function Get_Text (Comment : in Comment_Type;
                      Strip   : in Boolean := False) return String;

   --  Get the given line of the comment text.
   function Get_Text (Comment : in Comment_Type;
                      Line    : in Positive;
                      Strip   : in Boolean := False) return String;

   --  Definition of the list of comments found in the CSS document.
   type CSSComment_List is tagged limited private;
   
   --  Insert in the comment list the comment described by <tt>Text</tt>
   --  and associated with source line number <tt>Line</tt>.  After insertion
   --  the comment reference is returned to identify the new comment.
   procedure Insert (List  : in out CSSComment_List;
                     Text  : in String;
                     Line  : in Natural;
                     Index : out Comment_Type);

private

   type Comment;
   type Const_Comment_Access is access constant Comment;
   type Comment_Access is access all Comment;
   
   type Line_Info is record
      Start  : Natural := 0;
      Finish : Natural := 0;
   end record;
   
   type Line_Info_Array is array (Natural range <>) of Line_Info;

   type Comment (Len : Natural; Line_Count : Natural) is limited record
      Next  : Comment_Access;
      Line  : Natural := 0;
      Text  : String (1 .. Len);
      Lines : Line_Info_Array (1 .. Line_Count);
   end record;

   type CSSComment_List is limited new Ada.Finalization.Limited_Controlled with record
      Chain : Comment_Access;
   end record;

   --  Release the memory used by the comment list.
   overriding
   procedure Finalize (List : in out CSSComment_List);

   EMPTY_COMMENT : aliased constant Comment := Comment '(Len => 0, Line_Count => 0, Next => null,
                                                         Text => "", Line => 0, others => <>);

   type Comment_Type is record
      Object : Const_Comment_Access := EMPTY_COMMENT'Access;
   end record;

   NULL_COMMENT  : constant Comment_Type := Comment_Type '(Object => EMPTY_COMMENT'Access);

end CSS.Comments;
