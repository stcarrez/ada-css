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

package body CSS.Analysis.Rules is

   --  Set the min and max repeat for this rule.
   procedure Set_Repeat (Rule : in out Rule_Type;
                         Min  : in Natural;
                         Max  : in Natural) is
   begin
      Rule.Min_Repeat := Min;
      Rule.Max_Repeat := Max;
   end Set_Repeat;

   --  Check if the value matches the rule.
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return False;
   end Match;

   --  Check if the value matches the identifier defined by the rule.
   function Match (Rule  : in Ident_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return CSS.Core.Values.To_String (Value) = Rule.Ident;
   end Match;

   --  Check if the value matches one of the sub rules.
   function Match (Rule  : in Or_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return False;
   end Match;

   --  Check if the value matches the identifier defined by the rule.
   function Match (Rule  : in And_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return False;
   end Match;

   --  Create a property rule and add it to the repository under the given name.
   --  The rule is empty and is ready to be defined.
   procedure Create_Property (Repository : in out Repository_Type;
                              Name       : in String;
                              Rule       : out Rule_Type_Access) is
   begin
      null;
   end Create_Property;

   --  Create a rule definition and add it to the repository under the given name.
   --  The rule definition is used by other rules to represent complex rules.
   --  The rule is empty and is ready to be defined.
   procedure Create_Definition (Repository : in out Repository_Type;
                                Name       : in String;
                                Rule       : out Rule_Type_Access) is
   begin
      null;
   end Create_Definition;

   --  Create a rule that describes an identifier;
   function Create_Identifier (Name : in String) return Rule_Type_Access is
      Rule : constant Rule_Type_Access :=
         new Ident_Rule_Type '(Ada.Finalization.Limited_Controlled with
           Next => null, Len => Name'Length, Ident => Name, others => <>);
   begin
      return Rule;
   end Create_Identifier;

   --  Create a rule that describes either a definition of a pre-defined type.
   function Create_Definition (Repository : in Repository_Type;
                               Name       : in String) return Rule_Type_Access is
      Pos  : Rule_Maps.Cursor := Repository.Types.Find (Name);
   begin
      if Rule_Maps.Has_Element (Pos) then
         return Rule_Maps.Element (Pos);
      end if;
      return new Ident_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                   Next => null, Len => Name'Length, Ident => Name, others => <>);
   end Create_Definition;

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   function Create_Group (Rules : in Rule_Type_Access) return Rule_Type_Access is
   begin
      return new Or_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                Next => null, List => Rules, others => <>);
   end Create_Group;

end CSS.Analysis.Rules;
