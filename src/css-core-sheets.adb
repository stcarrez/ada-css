-----------------------------------------------------------------------
--  css-core-sheets -- CSS stylesheet representation
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
with CSS.Core.Refs;
with Util.Log.Locations;
package body CSS.Core.Sheets is

   --  ------------------------------
   --  Create a CSS rule.
   --  ------------------------------
   function Create_Rule (Document : in CSSStylesheet) return Styles.CSSStyleRule_Access is
      Result : constant Styles.CSSStyleRule_Access := new Styles.CSSStyleRule;
      File   : constant Util.Log.Locations.File_Info_Access := Document.File;
   begin
      Result.Style.Set_File_Info (File);
      return Result;
   end Create_Rule;

   --  ------------------------------
   --  Create a CSS font-face rule.
   --  ------------------------------
   function Create_Rule (Document : in CSSStylesheet) return Styles.CSSFontfaceRule_Access is
      Result : constant Styles.CSSFontfaceRule_Access := new Styles.CSSFontfaceRule;
      File   : constant Util.Log.Locations.File_Info_Access := Document.File;
   begin
      Result.Style.Set_File_Info (File);
      return Result;
   end Create_Rule;

   --  ------------------------------
   --  Create a CSS media rule.
   --  ------------------------------
   function Create_Rule (Document : in CSSStylesheet) return Medias.CSSMediaRule_Access is
      Result : constant Medias.CSSMediaRule_Access := new Medias.CSSMediaRule;
      --  File   : constant Util.Log.Locations.File_Info_Access := Document.File;
   begin
      --  Result.Style.Set_File_Info (File);
      return Result;
   end Create_Rule;

   --  ------------------------------
   --  Append the CSS rule to the document.
   --  ------------------------------
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Styles.CSSStyleRule_Access;
                     Line     : in Natural;
                     Column   : in Natural) is
      Ref : constant CSS.Core.Refs.Ref := CSS.Core.Refs.Create (Rule.all'Access);
   begin
      Rule.Set_Location (Line, Column, Document'Unchecked_Access);
      Document.Rules.Append (Ref);
   end Append;

   --  ------------------------------
   --  Append the CSS rule to the document.
   --  ------------------------------
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Styles.CSSFontfaceRule_Access;
                     Line     : in Natural;
                     Column   : in Natural) is
      Ref : constant CSS.Core.Refs.Ref := CSS.Core.Refs.Create (Rule.all'Access);
   begin
      Rule.Set_Location (Line, Column, Document'Unchecked_Access);
      Document.Rules.Append (Ref);
   end Append;

   --  ------------------------------
   --  Append the media rule to the document.
   --  ------------------------------
   procedure Append (Document : in out CSSStylesheet;
                     Media    : in Medias.CSSMediaRule_Access;
                     Rule     : in Styles.CSSStyleRule_Access;
                     Line     : in Natural;
                     Column   : in Natural) is
      use type Medias.CSSMediaRule_Access;

      Ref : constant CSS.Core.Refs.Ref := CSS.Core.Refs.Create (Rule.all'Access);
   begin
      Rule.Set_Location (Line, Column, Document'Unchecked_Access);
      if Media /= null then
         Media.Rules.Append (Ref);
      else
         Document.Rules.Append (Ref);
      end if;
   end Append;

   --  ------------------------------
   --  Append the CSS rule to the media.
   --  ------------------------------
   procedure Append (Document : in out CSSStylesheet;
                     Rule     : in Medias.CSSMediaRule_Access;
                     Line     : in Natural;
                     Column   : in Natural) is
      Ref : constant CSS.Core.Refs.Ref := CSS.Core.Refs.Create (Rule.all'Access);
   begin
      Rule.Set_Location (Line, Column, Document'Unchecked_Access);
      Document.Rules.Append (Ref);
   end Append;

   --  ------------------------------
   --  Iterate over the properties of each CSS rule.  The <tt>Process</tt> procedure
   --  is called with the CSS rule and the property as parameter.
   --  ------------------------------
   procedure Iterate_Properties (Document : in CSSStylesheet;
                                 Process  : not null access
                                   procedure (Rule     : in Styles.CSSStyleRule'Class;
                                              Property : in Properties.CSSProperty)) is
      use Styles;
      procedure Process_Rule (Pos : in CSS.Core.Vectors.Cursor);

      procedure Process_Rule (Pos : in CSS.Core.Vectors.Cursor) is
         procedure Process_Property (Prop : in Properties.CSSProperty);

         Rule : constant CSSStyleRule_Access := Styles.Element (Pos);

         procedure Process_Property (Prop : in Properties.CSSProperty) is
         begin
            Process (Rule.all, Prop);
         end Process_Property;
      begin
         if Rule /= null then
            Rule.Style.Iterate (Process_Property'Access);
         end if;
      end Process_Rule;

   begin
      Document.Rules.Iterate (Process_Rule'Access);
   end Iterate_Properties;

end CSS.Core.Sheets;
