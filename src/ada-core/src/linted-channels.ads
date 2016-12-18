-- Copyright 2016 Steven Stewart-Gallus
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
with Linted.Options;

generic
   type Element_T is private;
package Linted.Channels is
   pragma Pure;

   package Option_Element_Ts is new Linted.Options (Element_T);

   protected type Channel is
      --  Overwrites old values
      procedure Push (D : Element_T) with
	Global => null,
	Depends => (Channel => (D, Channel));
      entry Pop (D : out Element_T) with
	Global => null,
	Depends => (D => Channel, Channel => Channel);
      procedure Poll (Option : out Option_Element_Ts.Option) with
	Global => null,
	Depends => (Option => Channel, Channel => Channel);
   private
      Current : Element_T;
      Full : Boolean := False;
   end Channel;
end Linted.Channels;
