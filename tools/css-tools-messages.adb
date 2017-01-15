-----------------------------------------------------------------------
--  css-tools-messages -- CSS tools messages
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
with Ada.Unchecked_Deallocation;
package body CSS.Tools.Messages is

   --  ------------------------------
   --  Add a message for the given source location.
   --  ------------------------------
   procedure Add (Handler : in out Message_List;
                  Loc     : in CSS.Core.Location;
                  Message : in Message_Type_Access) is
      Pos : constant Message_Sets.Cursor := Handler.List.Find (Loc);
   begin
      if Message_Sets.Has_Element (Pos) then
         Message_Sets.Element (Pos).Next := Message;
      else
         Handler.List.Insert (Loc, Message);
      end if;
   end Add;

   --  ------------------------------
   --  Report an error message at the given CSS source position.
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Message_List;
                    Loc     : in CSS.Core.Location;
                    Message : in String) is
      Msg : constant Message_Type_Access
        := new Message_Type '(Len     => Message'Length,
                              Kind    => MSG_ERROR,
                              Next    => null,
                              Message => Message);
   begin
      Handler.Add (Loc, Msg);
   end Error;

   --  ------------------------------
   --  Report a warning message at the given CSS source position.
   --  ------------------------------
   overriding
   procedure Warning (Handler : in out Message_List;
                      Loc     : in CSS.Core.Location;
                      Message : in String) is
      Msg : constant Message_Type_Access
        := new Message_Type '(Len     => Message'Length,
                              Kind    => MSG_WARNING,
                              Next    => null,
                              Message => Message);
   begin
      Handler.Add (Loc, Msg);
   end Warning;

   --  ------------------------------
   --  Iterate over the list of message in the line order.
   --  ------------------------------
   procedure Iterate (List : in Message_List;
                      Process : not null access procedure (Severity : in Severity_Type;
                                                           Loc      : in CSS.Core.Location;
                                                           Message  : in String)) is
      Iter : Message_Sets.Cursor := List.List.First;
   begin
      while Message_Sets.Has_Element (Iter) loop
         declare
            Loc : CSS.Core.Location := Message_Sets.Key (Iter);
            Msg : Message_Type_Access := Message_Sets.Element (Iter);
         begin
            while Msg /= null loop
               Process (Msg.Kind, Loc, Msg.Message);
               Msg := Msg.Next;
            end loop;
         end;
         Message_Sets.Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Release the list of messages.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out Message_List) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Message_Type, Message_Type_Access);
   begin
      while not List.List.Is_Empty loop
         declare
            Iter : Message_Sets.Cursor := List.List.First;
            Msg  : Message_Type_Access := Message_Sets.Element (Iter);
         begin
            Free (Msg);
            List.List.Delete (Iter);
         end;
      end loop;
   end Finalize;

end CSS.Tools.Messages;
