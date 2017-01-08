-----------------------------------------------------------------------
--  css -- Ada CSS Library
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
with Ada.Strings.Unbounded;
with CSS.Core;
package CSS.Parser is

   type Unit_Type is (U_NONE, U_PX, U_EX, U_EM, U_CM, U_MM, U_IN, U_PI, U_PC, U_PT,
                      U_DEG, U_RAD, U_GRAD,
                      U_MS, U_SEC, U_HZ, U_KHZ);

   type Value_Type is (V_NONE, V_STRING, V_URL, V_NUMBER, V_IDENT, V_FUNCTION);

   procedure Load (Path  : in String;
                   Sheet : in CSS.Core.Stylesheet_Access);

   type YYstype is record
      Unit  : Unit_Type := U_NONE;
      Kind  : Value_Type := V_NONE;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Set_Value (Into  : in out YYstype;
                        Value : in String;
                        Kind  : in Value_Type);

   procedure Set_Value (Into  : in out YYstype;
                        Value : in String;
                        Unit  : in Unit_Type);

private

   procedure Error (Line    : in Natural;
                    Message : in String);

end CSS.Parser;
