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
with Ada.Containers.Indefinite_Ordered_Maps;
with CSS.Core.Values;

--  == Analysis Rules ==
--  The <tt>CSS.Analysis.Rules</tt> package defines the rules for the verification of
--  value properties.  The list of definition is stored in the rule repository.
--  Each rule is associated with a name.  The rule is represented as a tree whose nodes
--  define what is valid for a given property value.
package CSS.Analysis.Rules is

   type Rule_Type is limited new Ada.Finalization.Limited_Controlled with private;
   type Rule_Type_Access is access all Rule_Type'Class;

   --  Set the min and max repeat for this rule.
   procedure Set_Repeat (Rule : in out Rule_Type;
                         Min  : in Natural;
                         Max  : in Natural);

   --  Check if the value matches the rule.
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Rule that describes an identifier such as 'left' or 'right'.
   type Ident_Rule_Type (Len : Natural) is new Rule_Type with private;

   --  Check if the value matches the identifier defined by the rule.
   function Match (Rule  : in Ident_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Rule that describes a list of alternatives such as top | bottom | left | right
   type Or_Rule_Type is new Rule_Type with private;

   --  Check if the value matches one of the sub rules.
   function Match (Rule  : in Or_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   --  Rule that describes a list of alternatives such as left && 2em
   type And_Rule_Type is new Rule_Type with private;

   --  Check if the value matches the identifier defined by the rule.
   function Match (Rule  : in And_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean;

   type Repository_Type is limited new Ada.Finalization.Limited_Controlled with private;

   --  Create a property rule and add it to the repository under the given name.
   --  The rule is empty and is ready to be defined.
   procedure Create_Property (Repository : in out Repository_Type;
                              Name       : in String;
                              Rule       : out Rule_Type_Access);

   --  Create a rule definition and add it to the repository under the given name.
   --  The rule definition is used by other rules to represent complex rules.
   --  The rule is empty and is ready to be defined.
   procedure Create_Definition (Repository : in out Repository_Type;
                                Name       : in String;
                                Rule       : out Rule_Type_Access);

   --  Create a rule that describes an identifier;
   function Create_Identifier (Name : in String) return Rule_Type_Access;

   --  Create a rule that describes either a definition of a pre-defined type.
   function Create_Definition (Repository : in Repository_Type;
                               Name       : in String) return Rule_Type_Access;

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   function Create_Group (Rules : in Rule_Type_Access) return Rule_Type_Access;

private

   type Rule_Type is limited new Ada.Finalization.Limited_Controlled with record
      Next       : Rule_Type_Access;
      Min_Repeat : Natural := 0;
      Max_Repeat : Natural := 0;
   end record;

   type Ident_Rule_Type (Len : Natural) is new Rule_Type with record
      Ident : String (1 .. Len);
   end record;

   type Definition_Rule_Type (Len : Natural) is new Rule_Type with record
      Ident : String (1 .. Len);
      Def   : Rule_Type_Access;
   end record;

   type Or_Rule_Type is new Rule_Type with record
      List       : Rule_Type_Access;
   end record;

   type And_Rule_Type is new Rule_Type with record
      List       : Rule_Type_Access;
   end record;

   package Rule_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                  Element_Type => Rule_Type_Access,
                                                  "="          => "=",
                                                  "<"          => "<");

   type Repository_Type is limited new Ada.Finalization.Limited_Controlled with record
      --  The rule names with their definitions.  These rules are noted with: <name>.
      Rules      : Rule_Maps.Map;

      --  The property names which are valid and their associated rule.
      Properties : Rule_Maps.Map;

      --  The pre-defined and built-in rules used to represent the basic types (ex: <color>).
      Types      : Rule_Maps.Map;
   end record;

end CSS.Analysis.Rules;
