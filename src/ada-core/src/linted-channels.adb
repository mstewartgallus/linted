-- Copyright 2016,2017 Steven Stewart-Gallus
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
package body Linted.Channels is
   procedure Push (This : in out Channel; D : Element_T) is
   begin
      This.Push (D);
   end Push;

   procedure Pop (This : in out Channel; D : out Element_T) is
   begin
      This.Pop_Impl (D);
   end Pop;

   procedure Poll
     (This : in out Channel;
      Option : out Option_Element_Ts.Option)
   is
      D : Element_T;
      Init : Boolean;
   begin
      This.Poll (D, Init);
      if Init then
	 Option := (Empty => False, Data => D);
      else
	 Option := (Empty => True);
      end if;
   end Poll;

   protected body Channel is
      procedure Push (D : Element_T) is
      begin
         Current := D;
         Full := True;
      end Push;

      entry Pop_Impl (D : out Element_T) when Full is
      begin
         D := Current;
         Full := False;
      end Pop_Impl;

      procedure Poll (D : out Element_T; Init : out Boolean) is
	 Dummy : Element_T;
      begin
         if Full then
            Full := False;
	    D := Current;
	    Init := True;
         else
	    D := Dummy;
	    Init := False;
         end if;
      end Poll;
   end Channel;
end Linted.Channels;
