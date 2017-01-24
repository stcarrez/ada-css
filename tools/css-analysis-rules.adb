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

with Util.Log.Loggers;
with Util.Strings;

with CSS.Core.Styles;
with CSS.Core.Properties;
with CSS.Analysis.Rules.Types;
package body CSS.Analysis.Rules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("CSS.Analysis.Rules");

   --  ------------------------------
   --  Get the source location of the rule definition.
   --  ------------------------------
   function Get_Location (Rule : in Rule_Type) return Location is
   begin
      return Rule.Loc;
   end Get_Location;

   --  ------------------------------
   --  Set the min and max repeat for this rule.
   --  ------------------------------
   procedure Set_Repeat (Rule : in out Rule_Type;
                         Min  : in Natural;
                         Max  : in Natural;
                         Sep  : in Boolean := False) is
   begin
      Rule.Min_Repeat := Min;
      Rule.Max_Repeat := Max;
      Rule.Comma_Sep  := Sep;
   end Set_Repeat;

   --  ------------------------------
   --  Append the <tt>New_Rule</tt> at end of the rule's list.
   --  ------------------------------
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

   --  ------------------------------
   --  Check if the value matches the rule.
   --  ------------------------------
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      pragma Unreferenced (Rule, Value);
   begin
      return False;
   end Match;

   --  ------------------------------
   --  Check if the value matches the identifier defined by the rule.
   --  ------------------------------
   function Match (Rule  : in Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural is
      Count : constant Natural := Value.Get_Count;
      Match_Count : Natural := 0;
   begin
      for I in Pos .. Count loop
         if not Rule_Type'Class (Rule).Match (Value.Get_Value (I)) then
            return Match_Count;
         end if;
         Match_Count := Match_Count + 1;
         if Match_Count >= Rule.Max_Repeat then
            return Match_Count;
         end if;
      end loop;
      if Match_Count < Rule.Min_Repeat then
         return 0;
      end if;
      return Match_Count;
   end Match;

   --  Check if the value matches the identifier defined by the rule.
   overriding
   function Match (Rule  : in Ident_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return CSS.Core.Values.To_String (Value) = Rule.Ident;
   end Match;

   --  ------------------------------
   --  Find a rule that describes a property.
   --  Returns the rule or null if there is no rule for the property.
   --  ------------------------------
   function Find_Property (Repository : in Repository_Type;
                           Name       : in String) return Rule_Type_Access is
      Pos : constant Rule_Maps.Cursor := Repository.Properties.Find (Name);
   begin
      if Rule_Maps.Has_Element (Pos) then
         return Rule_Maps.Element (Pos);
      else
         Log.Info ("No property rule '{0}'", Name);
         return null;
      end if;
   end Find_Property;

   --  ------------------------------
   --  Create a property rule and add it to the repository under the given name.
   --  The rule is empty and is ready to be defined.
   --  ------------------------------
   procedure Create_Property (Repository : in out Repository_Type;
                              Name       : in String;
                              Rule       : in Rule_Type_Access) is
   begin
      if Repository.Properties.Contains (Name) then
         Log.Error ("{0}: Property '{1}' is already defined",
                    CSS.Core.To_String (Rule.Loc), Name);
      else
         Log.Info ("Adding property '{0}'", Name);
         Repository.Properties.Insert (Name, Rule);
      end if;
   end Create_Property;

   --  ------------------------------
   --  Create a rule definition and add it to the repository under the given name.
   --  The rule definition is used by other rules to represent complex rules.
   --  The rule is empty and is ready to be defined.
   --  ------------------------------
   procedure Create_Definition (Repository : in out Repository_Type;
                                Name       : in String;
                                Rule       : in Rule_Type_Access) is
   begin
      if Repository.Rules.Contains (Name) then
         Log.Error ("{0}: Rule '{1}' is already defined",
                    CSS.Core.To_String (Rule.Loc), Name);
      else
         Log.Info ("Adding rule '{0}'", Name);
         Repository.Rules.Insert (Name, Rule);
      end if;
   end Create_Definition;

   --  Create a rule that describes an identifier;
   function Create_Identifier (Name : in String;
                               Loc  : in Location) return Rule_Type_Access is
      Rule : constant Rule_Type_Access :=
         new Ident_Rule_Type '(Ada.Finalization.Limited_Controlled with
           Next => null, Len => Name'Length, Ident => Name, Loc => Loc, others => <>);
   begin
      Log.Debug ("Create rule identifier '{0}'", Name);
      return Rule;
   end Create_Identifier;

   --  ------------------------------
   --  Create a rule that describes either a definition or a pre-defined type.
   --  ------------------------------
   function Create_Definition (Repository : in out Repository_Type;
                               Name       : in String;
                               Loc        : in Location) return Rule_Type_Access is
      Pos  : Rule_Maps.Cursor := Repository.Types.Find (Name);
   begin
      if Rule_Maps.Has_Element (Pos) then
         Log.Debug ("Create rule type '{0}'", Name);
         return new Type_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                     Next => null, Len => 0, Rule => Rule_Maps.Element (Pos),
                                     Loc => Loc, others => <>);
      end if;

      Pos := Repository.Rules.Find (Name);
      if Rule_Maps.Has_Element (Pos) then
         Log.Debug ("Create rule definition '{0}'", Name);
         return new Definition_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                           Next => null, Len => Name'Length,
                                           Ident => Name,
                                           Rule => Rule_Maps.Element (Pos),
                                           Loc => Loc, others => <>);
      end if;
      Log.Debug ("{0}: Create unknown rule definition '{1}'",
                 CSS.Core.To_String (Loc), Name);
      declare
         Rule : constant Definition_Rule_Type_Access
           := new Definition_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                         Next => null, Len => Name'Length,
                                         Rule => null,
                                         Ident => Name, Loc => Loc, others => <>);
      begin
         Repository.Deferred.Append (Rule);
         return Rule.all'Access;
      end;
   end Create_Definition;

   --  Check if the value matches the type.
   overriding
   function Match (Rule  : in Type_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      return Rule.Rule.Match (Value);
   end Match;

   --  Check if the value matches the type.
   overriding
   function Match (Rule  : in Definition_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
   begin
      if Rule.Rule = null then
         return False;
      else
         return Rule.Rule.Match (Value);
      end if;
   end Match;

   overriding
   function Match (Rule  : in Definition_Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural is
   begin
      if Rule.Rule = null then
         return 0;
      else
         return Rule.Rule.Match (Value, Pos);
      end if; 
   end Match;

   --  Check if the value matches the rule.
   overriding
   function Match (Rule  : in Group_Rule_Type;
                   Value : in CSS.Core.Values.Value_Type) return Boolean is
      List : Rule_Type_Access := Rule.List;
   begin
      if Rule.Kind = GROUP_ONLY_ONE then
         while List /= null loop
            if List.Match (Value) then
               return True;
            end if;
            List := List.Next;
         end loop;
      end if;
      return False;
   end Match;

   --  Check if the value matches the identifier defined by the rule.
   overriding
   function Match (Group : in Group_Rule_Type;
                   Value : in CSS.Core.Values.Value_List;
                   Pos   : in Positive := 1) return Natural is
      Count       : constant Natural := Value.Get_Count;
      Rule        : Rule_Type_Access;
      Match_Count : Natural := 0;
      N           : Natural;
      Repeat      : Natural := 0;
      Cur_Pos     : Positive := Pos;
   begin
      if Group.Kind = GROUP_ONLY_ONE then
         loop
            Rule := Group.List;
            while Rule /= null loop
               N := Rule.Match (Value, Cur_Pos);
               if N > 0 then
                  Repeat := Repeat + 1;
                  Match_Count := Match_Count + N;
                  Cur_Pos := Cur_Pos + N;
                  exit;
               end if;
               Rule := Rule.Next;
            end loop;
            if Rule = null or Repeat = Group.Max_Repeat or Cur_Pos > Count then
               if Repeat < Group.Min_Repeat then
                  return 0;
               end if;
               if Repeat > Group.Max_Repeat then
                  return 0;
               end if;
               return Match_Count;
            end if;
         end loop;

      elsif Group.Kind = GROUP_AND then
         declare
            M : Rule_Type_Access_Array (1 .. Group.Count);
            I : Positive := 1;
         begin
            while Cur_Pos <= Count loop
               Rule := Group.List;
               while Rule /= null loop
                  N := Rule.Match (Value, Cur_Pos);
                  if N > 0 then
                     Cur_Pos := Cur_Pos + N;
                     M (I) := Rule;
                     I := I + 1;
                     exit;
                  end if;
                  Rule := Rule.Next;
               end loop;
               exit when N = 0 or I = M'Last + 1;
               Match_Count := Match_Count + N;
            end loop;
            for I in M'Range loop
               for J in I + 1 .. M'Last loop
                  if M (I) = M (J) then
                     return 0;
                  end if;
               end loop;
            end loop;
            return Match_Count;
         end;

      elsif Group.Kind = GROUP_DBAR then
         declare
            M : Rule_Type_Access_Array (1 .. Group.Count);
            N : Natural;
            Cnt : Natural := 0;
            Found : Boolean;
         begin
            Rule := Group.List;
            while Rule /= null loop
               Cnt := Cnt + 1;
               M (Cnt) := Rule;
               Rule := Rule.Next;
            end loop;
            while Cur_Pos <= Count loop
               Found := False;
               for I in 1 .. Cnt loop
                  Rule := M (I);
                  N := Rule.Match (Value, Cur_Pos);
                  if N > 0 then
                     Match_Count := Match_Count + N;
                     if I /= Cnt then
                        M (I .. Cnt - 1) := M (I + 1 .. Cnt);
                     end if;
                     Cnt := Cnt - 1;
                     Cur_Pos := Cur_Pos + N;
                     Found := True;
                     exit;
                  end if;
               end loop;
               exit when Cnt = 0 or not Found;
            end loop;
            return Match_Count;
         end;

      elsif Group.Kind = GROUP_SEQ then
         declare
            I : Positive := 1;
            N : Natural;
         begin
            Rule := Group.List;
            while Cur_Pos <= Count loop
               exit when Rule = null;
               N := Rule.Match (Value, Cur_Pos);
               if N = 0 and Rule.Min_Repeat > 0 then
                  return 0;
               end if;
               Match_Count := Match_Count + N;
               Cur_Pos := Cur_Pos + N;
               Rule := Rule.Next;
            end loop;
            return Match_Count;
         end;
      end if;
      return Match_Count;
   end Match;

   --  Create a rule that describes a group of rules whose head is passed in <tt>Rules</tt>.
   procedure Append_Group (Into   : out Rule_Type_Access;
                           First  : in Rule_Type_Access;
                           Second : in Rule_Type_Access;
                           Kind   : in Group_Type) is
      Count : Natural := 1;
      Rule  : Rule_Type_Access := Second;
   begin
      while Rule /= null loop
         Count := Count + 1;
         Rule := Rule.Next;
      end loop;
      if First.all in Group_Rule_Type'Class and then Group_Rule_Type (First.all).Kind = Kind then
         Append (Group_Rule_Type (First.all).List.all, Second);
         Group_Rule_Type (First.all).Count := Group_Rule_Type (First.all).Count + Count;
         Into := First;
      else
         Into := new Group_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                       Next => null, List => First,
                                       Kind => Kind,
                                       Loc  => First.Loc,
                                       Count      => Count,
                                       Min_Repeat => 1,
                                       Max_Repeat => 1, others => <>);
         First.Next := Second;
      end if;
      Log.Debug ("Create rule group");
   end Append_Group;

   --  ------------------------------
   --  Create a rule that describes a function call with parameters.
   --  ------------------------------
   function Create_Function (Name   : in String;
                             Params : in Rule_Type_Access;
                             Loc    : in Location) return Rule_Type_Access is
   begin
      return new Function_Rule_Type '(Ada.Finalization.Limited_Controlled with
                                      Len    => Name'Length,
                                      Loc    => Loc,
                                      Kind   => GROUP_PARAMS,
                                      Next   => null,
                                      List   => Params,
                                      Ident  => Name,
                                      others => <>);
   end Create_Function;

   Repo : aliased Repository_Type;

   function Rule_Repository return access Repository_Type is
   begin
      return Repo'Access;
   end Rule_Repository;

   --  Print the rule definition to the print stream.
   procedure Print (Stream : in out CSS.Printer.File_Type'Class;
                    Rule   : in Rule_Type'Class) is
      List : Rule_Type_Access;
      Kind : Group_Type;
   begin
      if Rule in Group_Rule_Type'Class then
         List := Group_Rule_Type'Class (Rule).List;
         Kind := Group_Rule_Type'Class (Rule).Kind;
         if Rule in Function_Rule_Type'Class then
            Stream.Print (Function_Rule_Type (Rule).Ident);
         end if;
         if Kind = GROUP_PARAMS then
            Stream.Print ("(");
         else
            Stream.Print ("[");
         end if;
         while List /= null loop
            Print (Stream, List.all);
            if List.Next /= null then
               case Kind is
                  when GROUP_ONLY_ONE =>
                     Stream.Print (" | ");

                  when GROUP_DBAR =>
                     Stream.Print (" || ");

                  when GROUP_AND =>
                     Stream.Print (" && ");

                  when GROUP_PARAMS =>
                     Stream.Print (", ");

                  when GROUP_SEQ =>
                     Stream.Print (" ");

               end case;
            end if;
            List := List.Next;
         end loop;
      end if;
      if Rule in Ident_Rule_Type'Class then
         Stream.Print (Ident_Rule_Type (Rule).Ident);
      end if;
      if Rule in Group_Rule_Type'Class then
         if Kind = GROUP_PARAMS then
            Stream.Print (")");
         else
            Stream.Print ("]");
         end if;
      end if;
      if Rule.Comma_Sep then
         Stream.Print ("#");
         if Rule.Min_Repeat /= 1 and Rule.Max_Repeat /= 1 then
            Stream.Print ("{");
            Stream.Print (Util.Strings.Image (Rule.Min_Repeat));
            if Rule.Min_Repeat /= Rule.Max_Repeat then
               Stream.Print (",");
               Stream.Print (Util.Strings.Image (Rule.Max_Repeat));
            end if;
            Stream.Print ("}");
         end if;
      elsif Rule.Min_Repeat = 0 and Rule.Max_Repeat = 1 then
         Stream.Print ("?");
      elsif Rule.Min_Repeat = 1 and Rule.Max_Repeat = Natural'Last then
         Stream.Print ("+");
      elsif Rule.Min_Repeat = 0 and Rule.Max_Repeat = Natural'Last then
         Stream.Print ("*");
      elsif Rule.Min_Repeat /= 1 and Rule.Max_Repeat /= 1 then
         Stream.Print ("{");
         Stream.Print (Util.Strings.Image (Rule.Min_Repeat));
         if Rule.Min_Repeat /= Rule.Max_Repeat then
            Stream.Print (",");
            if Rule.Max_Repeat /= Natural'Last then
               Stream.Print (Util.Strings.Image (Rule.Max_Repeat));
            end if;
         end if;
         Stream.Print ("}");
      end if;
   end Print;

   procedure Resolve (Repository : in out Repository_Type) is
   begin
      while not Repository.Deferred.Is_Empty loop
         declare
            First : Rule_Vectors.Cursor := Repository.Deferred.First;
            Rule  : constant Definition_Rule_Type_Access := Rule_Vectors.Element (First);
            Pos   : constant Rule_Maps.Cursor := Repository.Rules.Find (Rule.Ident);
         begin
            if Rule_Maps.Has_Element (Pos) then
               Rule.Rule := Rule_Maps.Element (Pos);
            else
               Log.Error ("{0}: Unknown rule definition '{1}'",
                          CSS.Core.To_String (Rule.Loc), Rule.Ident);
            end if;
            Repository.Deferred.Delete (First);
         end;
      end loop;
   end Resolve;

   procedure Analyze (Sheet  : in CSS.Core.Sheets.CSSStylesheet;
                      Report : in out CSS.Core.Errors.Error_Handler'Class) is
      procedure Process (Rule : in CSS.Core.Styles.CSSStyleRule'Class;
                         Prop : in CSS.Core.Properties.CSSProperty);

      procedure Process (Rule : in CSS.Core.Styles.CSSStyleRule'Class;
                         Prop : in CSS.Core.Properties.CSSProperty) is
         pragma Unreferenced (Rule);

         R           : constant Rule_Type_Access := Repo.Find_Property (Prop.Name.all);
         Match_Count : Natural;
         Count       : Natural;
      begin
         if R = null then
            Report.Warning (Prop.Location, "Invalid property: " & Prop.Name.all);
         else
            Count := Prop.Value.Get_Count;
            Match_Count := R.Match (Prop.Value);
            if Match_Count /= Count then
               if Count = 1 then
                  Report.Warning (Prop.Location, "Invalid value '"
                                  & Prop.Value.To_String & "' for property " & Prop.Name.all);
               elsif Match_Count = 0 then
                  Report.Warning (Prop.Location, "Invalid values '"
                                  & Prop.Value.To_String & "' for property " & Prop.Name.all);
               elsif Match_Count = Count - 1 then
                  Report.Warning (Prop.Location, "Unexpected value '"
                                  & Prop.Value.To_String (Match_Count + 1, Positive'Last)
                                  & "' after '"
                                  & Prop.Value.To_String (1, Match_Count)
                                  & "' for property " & Prop.Name.all);
               else
                  Report.Warning (Prop.Location, "Unexpected values '"
                                  & Prop.Value.To_String (Match_Count + 1, Positive'Last)
                                  & "' after '"
                                  & Prop.Value.To_String (1, Match_Count)
                                  & "' for property " & Prop.Name.all);
               end if;
            end if;
         end if;
      end Process;
   begin
      Repo.Resolve;
      Sheet.Iterate_Properties (Process'Access);
   end Analyze;

   Int_Rule     : aliased Types.Integer_Rule_Type;
   Percent_Rule : aliased Types.Percentage_Rule_Type;
   Length_Rule  : aliased Types.Length_Rule_Type;
   Number_Rule  : aliased Types.Number_Rule_Type;
   Angle_Rule   : aliased Types.Angle_Rule_Type;
   String_Rule  : aliased Types.String_Rule_Type;
   URL_Rule     : aliased Types.URL_Rule_Type;
   Color_Rule   : aliased Types.Color_Rule_Type;
   Ident_Rule   : aliased Types.Identifier_Rule_Type;

begin
   Repo.Types.Insert ("<angle>", Angle_Rule'Access);
   Repo.Types.Insert ("<integer>", Int_Rule'Access);
   Repo.Types.Insert ("<number>", Number_Rule'Access);
   Repo.Types.Insert ("<length>", Length_Rule'Access);
   Repo.Types.Insert ("<percentage>", Percent_Rule'Access);
   Repo.Types.Insert ("<string>", String_Rule'Access);
   Repo.Types.Insert ("<url>", URL_Rule'Access);
   Repo.Types.Insert ("<hex-color>", Color_Rule'Access);
   Repo.Types.Insert ("<custom-ident>", Ident_Rule'Access);
end CSS.Analysis.Rules;
