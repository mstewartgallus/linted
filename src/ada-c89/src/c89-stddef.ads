pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C;

package C89.Stddef is
   pragma Pure;

   --  unsupported macro: NULL __null
   --  arg-macro: procedure offsetof (TYPE, MEMBER)
   --    __builtin_offsetof (TYPE, MEMBER)
   subtype ptrdiff_t is Interfaces.C.long;  -- /usr/lib/gcc/x86_64-linux-gnu/4.6/include/stddef.h:150

   subtype size_t is Interfaces.C.size_t;  -- /usr/lib/gcc/x86_64-linux-gnu/4.6/include/stddef.h:212
end C89.Stddef;
