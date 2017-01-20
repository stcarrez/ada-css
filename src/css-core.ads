-----------------------------------------------------------------------
--  css-core -- Core CSS API definition
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
with Util.Strings;
with Util.Refs;
with Util.Log.Locations;
private with CSS.Comments;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Maps;

--  The API implemented by the <tt>CSS.Core</tt> package and child packages try to
--  follow the IDL specification defined in the W3C CSS Object Model (CSSOM)
--  (See https://drafts.csswg.org/cssom/ or https://www.w3.org/TR/2016/WD-cssom-1-20160317/)
package CSS.Core is

   type CSSProperty_Name is access all String;
   subtype CSSProperty_Value is CSSProperty_Name;
   subtype Location is Util.Log.Locations.Line_Info;
   use type Util.Log.Locations.Line_Info;

   --  Get the line number.
   function Get_Line (Loc : in Location) return Natural renames Util.Log.Locations.Line;

   --  Get a printable representation of the source file name and line number.
   function To_String (Loc : in Location) return String;

   --  The StyleSheet interface represents an abstract, base style sheet.
   --  See CSSOM: 5.1.1. The StyleSheet Interface
   type Stylesheet is tagged limited private;
   type Stylesheet_Access is access all Stylesheet'Class;

   --  Returns the CSS type ("text/css").
   function Get_Type (Sheet : in Stylesheet) return String;

   --  Get the parent CSS stylesheet if there is one or return null.
   function Get_Parent (Sheet : in Stylesheet) return Stylesheet_Access;

   --  Get the source file information.
   function Get_File_Info (Sheet : in Stylesheet) return Util.Log.Locations.File_Info_Access;

   --  Get the href attribute (stylesheet location).
   function Get_Href (Sheet : in Stylesheet) return String;

   --  Set the href attribute representing the stylesheet location.
   procedure Set_Href (Sheet : in out Stylesheet;
                       Href  : in String);

   function Create_Property_Name (Sheet : in Stylesheet;
                                  Name  : in String) return CSSProperty_Name;

   --  Create a location record to represent a CSS source position.
   function Create_Location (Sheet  : in Stylesheet_Access;
                             Line   : in Natural;
                             Column : in Natural) return Location;

   type CSSRule_Type is (STYLE_RULE, CHARSET_RULE, IMPORT_RULE, MEDIA_RULE,
                         FONT_FACE_RULE, PAGE_RULE, MARGIN_RULE, NAMESPACE_RULE);

   --  The CSSRule interface represents an abstract, base CSS style rule.
   --  Each distinct CSS style rule type is represented by a distinct interface that inherits
   --  from this interface.
   --  See CSSOM: Section 5.4.2. The CSSRule Interface
   type CSSRule is abstract new Util.Refs.Ref_Entity with private;
   type CSSRule_Access is access all CSSRule'Class;

   --  Get the type that identifies the rule.
   function Get_Type (Rule : in CSSRule) return CSSRule_Type is abstract;

   --  Get the parent rule.  Returns null when there is no parent.
   function Get_Parent (Rule : in CSSRule) return CSSRule_Access;

   --  Get the stylesheet.
   function Get_Stylesheet (Rule : in CSSRule) return Stylesheet_Access;

   --  Get the location of the rule.
   function Get_Location (Rule : in CSSRule) return Location;

private

   type String_Access is access all String;

   package String_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Util.Strings.Name_Access,
      Element_Type    => CSSProperty_Name,
      Hash            => Util.Strings.Hash,
      Equivalent_Keys => Util.Strings.Equivalent_Keys);
   type String_Map_Access is access all String_Map.Map;

   type Stylesheet is new Ada.Finalization.Limited_Controlled with record
      Loc      : Location;
      Parent   : Stylesheet_Access;
      File     : Util.Log.Locations.File_Info_Access;
      Href     : Ada.Strings.Unbounded.Unbounded_String;
      Strings  : String_Map_Access := new String_Map.Map;
      Comments : CSS.Comments.CSSComment_List;
   end record;

   overriding
   procedure Finalize (Sheet : in out Stylesheet);

   type CSSRule is abstract new Util.Refs.Ref_Entity with record
      Loc      : Location;
      Sheet    : Stylesheet_Access;
      Parent   : CSSRule_Access;
      Comments : CSS.Comments.CSSComment_List;
   end record;

   --  Set the source code location.
   procedure Set_Location (Rule   : in out CSSRule'Class;
                           Line   : in Natural;
                           Column : in Natural;
                           Sheet  : in Stylesheet_Access);

end CSS.Core;
