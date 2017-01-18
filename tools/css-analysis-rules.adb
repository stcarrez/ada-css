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
with Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;
with Util.Log.Loggers;
with CSS.Core.Values;

package body CSS.Analysis.Rules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("CSS.Analysis.Rules");

   procedure Print (Rule : in Rule_Type'Class;
                    Indent : in Natural := 0);

   --  Get the source location of the rule definition.
   function Get_Location (Rule : in Rule_Type) return CSS.Core.Location is
   begin
      return Rule.Loc;
   end Get_Location;

   --  Set the min and max repeat for this rule.
   procedure Set_Repeat (Rule : in out Rule_Type;
                         Min  : in Natural;
                         Max  : in Natural) is
   begin
      Rule.Min_Repeat := Min;
      Rule.Max_Repeat := Max;
   end Set_Repeat;

   --  Append the <tt>New_Rule</tt> at end of the rule's list.
   procedure Append (Rule     : in out Rule_Type;
                     New_Rule : in Rule_Type_Access) is
      Tail : Rule_Type_Access := Rule.Next;
   begin
      if Tail = null then
         Rule.Next := New_Rule;
      else
         while Tail.Next /= null loop
            Tail := Tail.Next;
         end loop;
         Tail.Next := New_Rule;
      end if;
   end Append;

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
                              Rule       : in Rule_Type_Access) is
   begin
      if Repository.Properties.Contains (Name) then
         Log.Error ("Property '{0}' is already defined", Name);
      else
         Log.Info ("Adding property '{0}'", Name);
         Repository.Properties.Insert (Name, Rule);
         Ada.Text_IO.Put ("Property: " & Name & " := ");
         Print (Rule.all);
         Ada.Text_IO.New_Line;
      end if;
   end Create_Property;

   --  Create a rule definition and add it to the repository under the given name.
   --  The rule definition is used by other rules to represent complex rules.
   --  The rule is empty and is ready to be defined.
   procedure Create_Definition (Repository : in out Repository_Type;
                                Name       : in String;
                                Rule       : in Rule_Type_Access) is
   begin
      if Repository.Rules.Contains (Name) then
         Log.Error ("Rule '{0}' is already defined", Name);
      else
         Log.Info ("Adding rule '{0}'", Name);
         Repository.Rules.Insert (Name, Rule);
         Ada.Text_IO.Put ("Definition: " & Name & " = ");
         Print (Rule.all);
         Ada.Text_IO.New_Line;
      end if;
   end Create_Definition;

   --  Create a rule that describes an identifier;
   function Create_Identifier (Name : in String;
                               Loc  : in CSS.Core.Location) return Rule_Type_Access is
      Rule : constant Rule_Type_Access :=
         new Ident_Rule_Type '(Ada.Finalization.Limited_Controlled with
           Next => null, Len => Name'Length, Ident => Name, Loc => Loc, others => <>);
   begin
      Log.Debug ("Create rule identifier '{0}'", Name);
      return Rule;
   end Create_Identifier;

   --  Create a rule that describes either a definition of a pre-defined type.
   function Create_Definition (Repository : in Repository_Type;
                               Name       : in String;
                               Loc        : in CSS.Core.Location) return Rule_Type_Access is
      Pos  : Rule_Maps.Cursor := Repository.Types.Find (Name);
   begin
      if Rule_Maps.Has_Element (Pos) then
         Log.Debug ("Create rule type '{0}'", Name);
         return Rule_Maps.Element (Pos);
      end if;
      Log.Debug ("Create rule definition '<{0}>'", Name);
      return new Ident_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                   Next => null, Len => Name'Length,
                                   Ident => Name, Loc => Loc, others => <>);
   end Create_Definition;

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   function Create_Group (Rules : in Rule_Type_Access;
                          Exc   : in Boolean) return Rule_Type_Access is
   begin
      Log.Debug ("Create rule group");
      return new Group_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                   Next => null, List => Rules,
                                   Loc  => Rules.Loc, others => <>);
   end Create_Group;

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   procedure Append_Group (Into   : out Rule_Type_Access;
                           First  : in Rule_Type_Access;
                           Second : in Rule_Type_Access;
                           Kind   : in Group_Type) is
   begin
      if First.all in Group_Rule_Type'Class and then Group_Rule_Type (First.all).Kind = Kind then
         Append (Group_Rule_Type (First.all).List.all, Second);
         Into := First;
      else
         Into := new Group_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                       Next => null, List => First,
                                       Kind => Kind,
                                       Loc  => First.Loc, others => <>);
         First.Next := Second;
      end if;
      Log.Debug ("Create rule group");
   end Append_Group;

   Repo : aliased Repository_Type;

   function Rule_Repository return access Repository_Type is
   begin
      return Repo'Access;
   end Rule_Repository;

   procedure Print (Rule : in Rule_Type'Class;
                    Indent : in Natural := 0) is
      List : Rule_Type_Access;
      Kind : Group_Type;
   begin
      if Rule in Group_Rule_Type'Class then
         List := Group_Rule_Type'Class (Rule).List;
         Kind := Group_Rule_Type'Class (Rule).Kind;
         if Kind = GROUP_PARAMS then
            Ada.Text_IO.Put ("(");
         else
            Ada.Text_IO.Put ("[");
         end if;
         while List /= null loop
            Print (List.all, Indent);
            if List.Next /= null then
               case Kind is
                  when GROUP_ONLY_ONE =>
                     Ada.Text_IO.Put (" | ");

                  when GROUP_DBAR =>
                     Ada.Text_IO.Put (" || ");

                  when GROUP_AND =>
                     Ada.Text_IO.Put (" && ");

                  when GROUP_PARAMS =>
                     Ada.Text_IO.Put (", ");

               end case;
            end if;
            List := List.Next;
         end loop;
      end if;
      if Rule in Ident_Rule_Type'Class then
         Ada.Text_IO.Put (Ident_Rule_Type (Rule).Ident);
      end if;
      if Rule in Group_Rule_Type'Class then
         if Kind = GROUP_PARAMS then
            Ada.Text_IO.Put (")");
         else
            Ada.Text_IO.Put ("]");
         end if;
      end if;
   end Print;

end CSS.Analysis.Rules;
