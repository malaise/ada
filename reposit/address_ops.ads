-- Operation on addresses
with System;
with Long_Longs;
package Address_Ops is

  -- Add an offset to an address
  function "+" (Addr : System.Address; Offset : Long_Longs.Ll_Integer)
               return System.Address;

  -- Delta between two addresses
  function "-" (Addr1, Addr2 : System.Address) return Long_Longs.Ll_Integer;

  -- Image of an address
  function Image (Addr : System.Address) return String;

end Address_Ops;

