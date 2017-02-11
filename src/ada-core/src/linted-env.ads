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
with Linted.Errors;

package Linted.Env with
     Spark_Mode,
     Initializes => Environment,
     Abstract_State =>
     (Environment with
      Synchronous,
      External => (Async_Readers, Async_Writers))
is
   pragma Preelaborate;

   procedure Set
     (Name : String;
      Value : String;
      Overwrite : Boolean;
      Err : out Errors.Error) with
      Global => (In_Out => Environment),
      Depends => ((Err, Environment) => (Environment, Name, Value, Overwrite));
   function Get (Name : String) return String with
      Volatile_Function,
      Global => (Input => Environment),
      Depends => (Get'Result => Environment);
end Linted.Env;
