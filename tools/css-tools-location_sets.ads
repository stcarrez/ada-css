-----------------------------------------------------------------------
--  css-tools-location_sets -- Sets of source location
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

with Util.Log.Locations;
with CSS.Core.Refs;
with CSS.Core.Compare;
with Ada.Containers.Ordered_Sets;
package CSS.Tools.Location_Sets is
   new Ada.Containers.Ordered_Sets (Element_Type => CSS.Core.Location,
                                    "<"          => Util.Log.Locations."<",
                                    "="          => Util.Log.Locations."=");
