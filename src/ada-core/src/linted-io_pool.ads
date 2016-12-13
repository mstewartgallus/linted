-- Copyright 2015,2016 Steven Stewart-Gallus
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
with Ada.Synchronous_Task_Control;
with Interfaces.C;
with System;

with Linted.Errors;
with Linted.KOs;
with Linted.Options;

package Linted.IO_Pool with SPARK_Mode => Off is
   pragma Elaborate_Body;
   use type Interfaces.C.int;

   type Writer_Event is record
      Bytes_Written : Interfaces.C.size_t;
      Err : Errors.Error;
   end record;

   package Option_Writer_Events is new Linted.Options (Writer_Event);

   generic
   package Writer_Worker is
      procedure Write (Object : KOs.KO; Buf : System.Address; Count : Interfaces.C.size_t);
      function Poll return Option_Writer_Events.Option;

      procedure Wait;
   end Writer_Worker;

   type Reader_Event is record
      Bytes_Read : Interfaces.C.size_t;
      Err : Errors.Error;
   end record;

   package Option_Reader_Events is new Linted.Options (Reader_Event);

   generic
   package Reader_Worker is
      procedure Read (Object : KOs.KO; Buf : System.Address; Count : Interfaces.C.size_t);
      function Poll return Option_Reader_Events.Option;
      procedure Wait;
   end Reader_Worker;

   type Poll_Events is (Poll_Events_Read, Poll_Events_Write);

   type Poller_Event is record
      Err : Errors.Error;
   end record;

   package Option_Poller_Events is new Linted.Options (Poller_Event);

   generic
   package Poller_Worker is
      procedure Watch (Object : KOs.KO; Events : Poll_Events);
      function Poll return Option_Poller_Events.Option;
      procedure Wait;
   end Poller_Worker;
end Linted.IO_Pool;
