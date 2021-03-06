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
with Interfaces.C;

with Libc.Stdint;

with Linted.Errors;

package Linted.GPU with
     Spark_Mode => Off is
   pragma Preelaborate;
   pragma Link_With ("-lgpu -lEGL -lm");

   subtype X11_Window is Libc.Stdint.uint_fast32_t;  -- lntd/gpu.h:29

   --  skipped empty struct lntd_gpu_context
   type Context is limited private;
   type Context_Access is access all Context;

   type Update is record
      Z_Rotation : Interfaces.C.C_float;  -- lntd/gpu.h:34
      X_Rotation : Interfaces.C.C_float;  -- lntd/gpu.h:35
      X_Position : Interfaces.C.C_float;  -- lntd/gpu.h:37
      Y_Position : Interfaces.C.C_float;  -- lntd/gpu.h:38
      Z_Position : Interfaces.C.C_float;  -- lntd/gpu.h:39
      MX_Position : Interfaces.C.C_float;  -- lntd/gpu.h:41
      MY_Position : Interfaces.C.C_float;  -- lntd/gpu.h:42
      MZ_Position : Interfaces.C.C_float;  -- lntd/gpu.h:43
   end record;

   function Context_Create
     (Con : out Context_Access) return Errors.Error;  -- lntd/gpu.h:47
   pragma Import (C, Context_Create, "lntd_gpu_context_create");

   function Context_Destroy
     (Con : Context_Access) return Linted.Errors.Error;  -- lntd/gpu.h:49
   pragma Import (C, Context_Destroy, "lntd_gpu_context_destroy");

   function Set_X11_Window
     (Con : Context_Access;
      Window : X11_Window) return Linted.Errors.Error;  -- lntd/gpu.h:51
   pragma Import (C, Set_X11_Window, "lntd_gpu_set_x11_window");

   function Remove_Window
     (Con : Context_Access) return Linted.Errors.Error;  -- lntd/gpu.h:53
   pragma Import (C, Remove_Window, "lntd_gpu_remove_window");

   procedure Update_State
     (Con : Context_Access;
      gpu_update : Update);  -- lntd/gpu.h:55
   pragma Import (C, Update_State, "lntd_gpu_update_state");

   procedure Resize
     (Con : Context_Access;
      width : Interfaces.C.unsigned;
      height : Interfaces.C.unsigned);  -- lntd/gpu.h:58
   pragma Import (C, Resize, "lntd_gpu_resize");

   procedure Hide (Con : Context_Access);  -- lntd/gpu.h:61
   pragma Import (C, Hide, "lntd_gpu_hide");

   procedure Show (Con : Context_Access);  -- lntd/gpu.h:62
   pragma Import (C, Show, "lntd_gpu_show");

private
   type Context is limited record
      null;
   end record;
end Linted.GPU;
