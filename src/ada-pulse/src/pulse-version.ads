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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Pulse.Version with
     Spark_Mode => Off is

   --  arg-macro: function pa_get_headers_version ()
   --    return "4.0.0";

   PA_API_VERSION : constant := 12;
   PA_PROTOCOL_VERSION : constant := 28;
   PA_MAJOR : constant := 4;
   PA_MINOR : constant := 0;
   PA_MICRO : constant := 0;

   --  arg-macro: function PA_CHECK_VERSION (major, minor, m((PA_MAJOR > (major))  or else  (PA_MAJOR = (major)  and then  PA_MINOR > (minor))  or else  (PA_MAJOR = (major)  and then  PA_MINOR = (minor)  and then  PA_MICRO >= (micro))
   --    return (PA_MAJOR > (major))  or else  (PA_MAJOR = (major)  and then  PA_MINOR > (minor))  or else  (PA_MAJOR = (major)  and then  PA_MINOR = (minor)  and then  PA_MICRO >= (micro));
   function pa_get_library_version
     return Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/version.h:42
   pragma Import (C, pa_get_library_version, "pa_get_library_version");

end Pulse.Version;
