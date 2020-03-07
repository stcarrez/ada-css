-----------------------------------------------------------------------
--  css-printer -- CSS printer tools
--  Copyright (C) 2017, 2020 Stephane Carrez
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
with CSS.Core.Properties;
with CSS.Core.Styles;
with CSS.Core.Sheets;
package CSS.Printer is

   type File_Type is abstract tagged limited record
      Indent       : Natural := 0;
      Indent_Level : Natural := 0;
      Compress     : Boolean := False;
      Full_Semi    : Boolean := False;
      Need_Semi    : Boolean := False;
   end record;

   procedure New_Line (Stream : in out File_Type) is abstract;

   procedure Print (Stream  : in out File_Type;
                    Content : in String) is abstract;

   procedure Print (Stream  : in out File_Type;
                    Content : in Character) is abstract;

   procedure Print (Stream : in out File_Type'Class;
                    Rule   : in CSS.Core.Styles.CSSStyleRule'Class);

   procedure Print (Stream : in out File_Type'Class;
                    Prop   : in CSS.Core.Properties.CSSProperty);

   procedure Print (Stream : in out File_Type'Class;
                    Sheet  : in CSS.Core.Sheets.CSSStylesheet);

   procedure Do_Indent (Stream : in out File_Type'Class);

end CSS.Printer;
