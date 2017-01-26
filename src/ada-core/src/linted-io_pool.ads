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
with Interfaces.C;
with System;

with Linted.Errors;
with Linted.KOs;

package Linted.IO_Pool with
     Spark_Mode is
   pragma Elaborate_Body;
   use type Interfaces.C.int;

   type Writer_Event is record
      Bytes_Written : Interfaces.C.size_t := 0;
      Err : Errors.Error;
   end record;

   type Reader_Event is record
      Bytes_Read : Interfaces.C.size_t := 0;
      Err : Errors.Error;
   end record;

   type Poller_Event_Type is (Readable, Writable);
   type Poller_Event_Set is array (Poller_Event_Type) of Boolean with
        Pack;
   type Poller_Event is record
      Events : Poller_Event_Set :=
        (Poller_Event_Type'First .. Poller_Event_Type'Last => False);
      Err : Errors.Error;
   end record;

   generic
      with procedure Notify (Event : Writer_Event);
   package Writer_Worker with
      Spark_Mode is
      procedure Write
        (Object : KOs.KO;
         Buf : System.Address;
         Count : Interfaces.C.size_t);
   end Writer_Worker;

   generic
      with procedure On_Event (Event : Reader_Event);
   package Reader_Worker with
      Spark_Mode is
      procedure Read
        (Object : KOs.KO;
         Buf : System.Address;
         Count : Interfaces.C.size_t);
   end Reader_Worker;

   generic
      with procedure On_Event (Event : Poller_Event);
   package Poller_Worker with
      Spark_Mode is
      procedure Poll (Object : KOs.KO; Events : Poller_Event_Set);
   end Poller_Worker;
end Linted.IO_Pool;
