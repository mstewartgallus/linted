-- Copyright 2017 Steven Stewart-Gallus
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
with Linted.Wait_Lists;
with Linted.Lock_Free_Stack;

package body Linted.Stack with
     Refined_State =>
     (State => (My_Stack.State, Buf_Has_Free_Space, Buf_Has_Contents))
is
   package My_Stack is new Lock_Free_Stack (Element_T, Ix, Is_Valid);
   Buf_Has_Free_Space : Wait_Lists.Wait_List;
   Buf_Has_Contents : Wait_Lists.Wait_List;

   procedure Push (Element : Element_T) with
      Refined_Global =>
      (In_Out => (Buf_Has_Free_Space, Buf_Has_Contents, My_Stack.State))
   is
      Init : Boolean;
   begin
      loop
         My_Stack.Try_Push (Element, Init);
         exit when Init;
         Wait_Lists.Wait (Buf_Has_Free_Space);
      end loop;
      Wait_Lists.Signal (Buf_Has_Contents);
   end Push;

   procedure Pop (Element : out Element_T) with
      Refined_Global =>
      (In_Out => (Buf_Has_Free_Space, Buf_Has_Contents, My_Stack.State))
   is
      Init : Boolean;
   begin
      loop
         My_Stack.Try_Pop (Element, Init);
         exit when Init;
         Wait_Lists.Wait (Buf_Has_Contents);
      end loop;
      Wait_Lists.Signal (Buf_Has_Free_Space);
   end Pop;

   procedure Try_Pop (Element : out Element_T; Success : out Boolean) is
   begin
      My_Stack.Try_Pop (Element, Success);
      if Success then
         Wait_Lists.Signal (Buf_Has_Free_Space);
      end if;
   end Try_Pop;

end Linted.Stack;
