with System;
package Address_Ops is

  function "+" (Addr : System.Address; Offset : Long_Long_Integer)
               return System.Address;
  function "-" (Addr1, Addr2 : System.Address) return Long_Long_Integer;

  function Image (Addr : System.Address) return String;

end Address_Ops;

