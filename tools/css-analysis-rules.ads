-----------------------------------------------------------------------
--  css-analysis-rules -- CSS Analysis Rules
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
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Vectors;

with CSS.Core.Errors;
with CSS.Core.Values;
with CSS.Printer;
with CSS.Core.Sheets;

--  == Analysis of CSS Rules ==
--  The <tt>CSS.Analysis.Rules</tt> package defines the rules for the verification of
--  value properties.  The list of definition is stored in the rule repository.
--  Each rule is associated with a name.  The rule is represented as a tree whose nodes
--  define what is valid for a given property value.
package CSS.Analysis.Rules is

   subtype Location is CSS.Core.Location;

   type Rule_Type is limited new Ada.Finalization.Limited_Controlled with private;
   type Rule_Type_Access is access all Rule_Type'Class;

   --  Get the source location of the rule definition.
   function Get_Location (Rule : in Rule_Type) return Location;

   --  Set the min and max repeat for this rule.
   procedure Set_Repeat (Rule : in out Rule_Type;
                         Min  : in Natural;
                         Max  : in Natural;
                         Sep  : in Boolean := False);

   --  Append the <tt>New_Rule</tt> at end of the rule's list.
   procedure Append (Rule     : in out Rule_Type;
                     New_Rule : in Rule_Type_Access);

   --  Print the rule definition to the print stream.
   procedure Print (Rule   : in Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class);

   --  Check if the value matches the rule.
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Check if the value matches the identifier defined by the rule.
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural;

   --  Rule that describes an identifier such as 'left' or 'right'.
   type Ident_Rule_Type (Len : Natural) is new Rule_Type with private;

   --  Print the rule definition to the print stream.
   overriding
   procedure Print (Rule   : in Ident_Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class);

   --  Check if the value matches the identifier defined by the rule.
   overriding
   function Match (Rule  : in Ident_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Repository_Type is limited new Ada.Finalization.Limited_Controlled with private;

   --  Find a rule that describes a property.
   --  Returns the rule or null if there is no rule for the property.
   function Find_Property (Repository : in Repository_Type;
                           Name       : in String) return Rule_Type_Access;

   --  Create a property rule and add it to the repository under the given name.
   --  The rule is empty and is ready to be defined.
   procedure Create_Property (Repository : in out Repository_Type;
                              Name       : in String;
                              Rule       : in Rule_Type_Access);

   --  Create a rule definition and add it to the repository under the given name.
   --  The rule definition is used by other rules to represent complex rules.
   --  The rule is empty and is ready to be defined.
   procedure Create_Definition (Repository : in out Repository_Type;
                                Name       : in String;
                                Rule       : in Rule_Type_Access);

   --  Create a rule that describes an identifier;
   function Create_Identifier (Name : in String;
                               Loc  : in Location) return Rule_Type_Access;

   --  Create a rule that describes either a definition or a pre-defined type.
   function Create_Definition (Repository : in out Repository_Type;
                               Name       : in String;
                               Loc        : in Location) return Rule_Type_Access;

   type Group_Type is (GROUP_ONLY_ONE, GROUP_DBAR, GROUP_AND, GROUP_SEQ, GROUP_PARAMS);

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   procedure Append_Group (Into   : out Rule_Type_Access;
                           First  : in Rule_Type_Access;
                           Second : in Rule_Type_Access;
                           Kind   : in Group_Type);

   --  Create a rule that describes a function call with parameters.
   function Create_Function (Name   : in String;
                             Params : in Rule_Type_Access;
                             Loc    : in Location) return Rule_Type_Access;

   procedure Analyze (Repository : in out Repository_Type;
                      Sheet      : in CSS.Core.Sheets.CSSStylesheet;
                      Report     : in out CSS.Core.Errors.Error_Handler'Class);

   --  Print the repository rule definitions to the print stream.
   procedure Print (Stream     : in out CSS.Printer.File_Type'Class;
                    Repository : in Repository_Type);

private

   type Rule_Type_Access_Array is array (Positive range <>) of Rule_Type_Access;

   type Rule_Type is limited new Ada.Finalization.Limited_Controlled with record
      Used       : Natural := 0;
      Loc        : Location;
      Next       : Rule_Type_Access;
      Min_Repeat : Natural := 0;
      Max_Repeat : Natural := 0;
      Comma_Sep  : Boolean := False;
   end record;

   overriding
   procedure Finalize (Rule : in out Rule_Type);

   type Type_Rule_Type (Len : Natural) is new Rule_Type with record
      Rule : Rule_Type_Access;
   end record;

   --  Check if the value matches the type.
   overriding
   function Match (Rule  : in Type_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Ident_Rule_Type (Len : Natural) is new Rule_Type with record
      Ident : String (1 .. Len);
   end record;

   type Definition_Rule_Type (Len : Natural) is new Rule_Type with record
      Ident : String (1 .. Len);
      Rule  : Rule_Type_Access;
   end record;
   type Definition_Rule_Type_Access is access all Definition_Rule_Type'Class;

   --  Print the rule definition to the print stream.
   overriding
   procedure Print (Rule   : in Definition_Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class);

   --  Check if the value matches the rule.
   overriding
   function Match (Rule  : in Definition_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Check if the value matches the identifier defined by the rule.
   overriding
   function Match (Rule  : in Definition_Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural;

   type Group_Rule_Type is new Rule_Type with record
      List       : Rule_Type_Access;
      Count      : Natural := 0;
      Kind       : Group_Type;
   end record;

   --  Print the rule definition to the print stream.
   overriding
   procedure Print (Rule   : in Group_Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class);

   --  Check if the value matches the rule.
   overriding
   function Match (Rule  : in Group_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Check if the value matches the identifier defined by the rule.
   overriding
   function Match (Group : in Group_Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural;

   overriding
   procedure Finalize (Rule : in out Group_Rule_Type);

   type Function_Rule_Type (Len : Natural) is new Group_Rule_Type with record
      Ident : String (1 .. Len);
   end record;

   --  Print the rule definition to the print stream.
   overriding
   procedure Print (Rule   : in Function_Rule_Type;
                    Stream : in out CSS.Printer.File_Type'Class);

   package Rule_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                  Element_Type => Rule_Type_Access,
                                                  "="          => "=",
                                                  "<"          => "<");

   package Rule_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Definition_Rule_Type_Access);

   type Repository_Type is limited new Ada.Finalization.Limited_Controlled with record
      --  The rule names with their definitions.  These rules are noted with: <name>.
      Rules      : Rule_Maps.Map;

      --  The property names which are valid and their associated rule.
      Properties : Rule_Maps.Map;

      --  The pre-defined and built-in rules used to represent the basic types (ex: <color>).
      Types      : Rule_Maps.Map;

      --  The list of definition rules that have not been found and must be
      --  resolved before doing the analysis.
      Deferred   : Rule_Vectors.Vector;
   end record;

   procedure Resolve (Repository : in out Repository_Type);

   overriding
   procedure Initialize (Repository : in out Repository_Type);

   --  Release the rules allocated dynamically.
   overriding
   procedure Finalize (Repository : in out Repository_Type);

   --  Erase all the rules that have been loaded in the repository.
   procedure Clear (Repository : in out Repository_Type);
   procedure Clear (Rules : in out Rule_Maps.Map);

end CSS.Analysis.Rules;
