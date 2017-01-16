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
with Ada.Finalization;
with CSS.Core.Errors;
private with Ada.Containers.Ordered_Maps;
package CSS.Tools.Messages is

   --  The message severity.
   type Severity_Type is (MSG_ERROR, MSG_WARNING, MSG_INFO);

   --  The list of messages for each source location.
   type Message_List is limited new Ada.Finalization.Limited_Controlled
     and CSS.Core.Errors.Error_Handler with private;

   --  Report an error message at the given CSS source position.
   overriding
   procedure Error (Handler : in out Message_List;
                    Loc     : in CSS.Core.Location;
                    Message : in String);

   --  Report a warning message at the given CSS source position.
   overriding
   procedure Warning (Handler : in out Message_List;
                      Loc     : in CSS.Core.Location;
                      Message : in String);

   --  Get the number of errors collected.
   function Get_Error_Count (Handler : in Message_List) return Natural;

   --  Get the number of warnings collected.
   function Get_Warning_Count (Handler : in Message_List) return Natural;

   --  Iterate over the list of message in the line order.
   procedure Iterate (List : in Message_List;
                      Process : not null access procedure (Severity : in Severity_Type;
                                                           Loc      : in CSS.Core.Location;
                                                           Message  : in String));

private

   type Message_Type;
   type Message_Type_Access is access Message_Type;
   type Message_Type (Len : Natural) is limited record
      Next    : Message_Type_Access;
      Kind    : Severity_Type := MSG_ERROR;
      Message : String (1 .. Len);
   end record;

   package Message_Sets is
     new Ada.Containers.Ordered_Maps (Key_Type     => CSS.Core.Location,
                                      Element_Type => Message_Type_Access,
                                      "<"          => CSS.Core."<",
                                      "="          => "=");

   --  The message list is a ordered map of message list.  The map is sorted on the
   --  line number.
   type Message_List is limited new Ada.Finalization.Limited_Controlled
     and CSS.Core.Errors.Error_Handler with record
      List          : Message_Sets.Map;
      Error_Count   : Natural := 0;
      Warning_Count : Natural := 0;
   end record;

   --  Add a message for the given source location.
   procedure Add (Handler : in out Message_List;
                  Loc     : in CSS.Core.Location;
                  Message : in Message_Type_Access);

   --  Release the list of messages.
   overriding
   procedure Finalize (List : in out Message_List);

end CSS.Tools.Messages;
