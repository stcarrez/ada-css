-----------------------------------------------------------------------
--  css-core-errors -- Error handler interface
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

--  === Error Handler ===
--  The <tt>Error_Handler</tt> interface represents the operations that must be implemented
--  by an error handler to get the error and warning message reported by the CSS parser.
--  The CSS source position is represented by the <tt>Location</tt> type that describes
--  the CSS stylesheet, the source line number and column number where the error or message
--  is reported.
package CSS.Core.Errors is

   type Error_Handler is limited interface;
   type Error_Handler_Access is access all Error_Handler'Class;

   --  Report an error message at the given CSS source position.
   procedure Error (Handler : in out Error_Handler;
                    Loc     : in Location;
                    Message : in String) is abstract;

   --  Report a warning message at the given CSS source position.
   procedure Warning (Handler : in out Error_Handler;
                      Loc     : in Location;
                      Message : in String) is abstract;

end CSS.Core.Errors;
