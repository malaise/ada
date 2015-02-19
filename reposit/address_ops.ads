-- Operation on addresses
with System;
with Long_Longs;
package Address_Ops is

  -- Add an offset to an address
  function "+" (Addr : System.Address; Offset : Long_Longs.Llu_Natural)
               return System.Address;
  function "-" (Addr : System.Address; Offset : Long_Longs.Llu_Natural)
               return System.Address;

  -- Delta between two addresses
  -- Raises Constraint_Error if Addr1 < Addr2
  function "-" (Addr1, Addr2 : System.Address) return Long_Longs.Llu_Natural;

  -- Image of an address
  function Image (Addr : System.Address) return String;

end Address_Ops;

