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
package body Linted.Channels with
     Spark_Mode => Off is
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
   begin
      This.Poll (Option);
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

      procedure Poll (Option : out Option_Element_Ts.Option) is
      begin
         if Full then
            Option := (Empty => False, Data => Current);
            pragma Annotate
              (Gnatprove,
               False_Positive,
               "discriminant check might fail",
               "reviewed by Steven Stewart-Gallus");
            Full := False;
         else
            Option := (Empty => True);
            pragma Annotate
              (Gnatprove,
               False_Positive,
               "discriminant check might fail",
               "reviewed by Steven Stewart-Gallus");
         end if;
      end Poll;
   end Channel;
end Linted.Channels;
