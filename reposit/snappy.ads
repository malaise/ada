with C_Types;
-- Snappy compression
package Snappy is

  -- An array of bytes to (un)compress
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;

  -- Compress Input into Output
  -- Outlen is at most one extra byte each 31 bytes, 3.3% overhead
  -- May raise Too_Big if Output is too small
  procedure Compress (Input  : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Natural);

  -- Uncompress Input into Output
  -- May raise Too_Big if Output is too small
  procedure Uncompress (Input  : in Byte_Array;
                        Output : out Byte_Array;
                        Outlen : out Natural);

  -- Output is too small or an offset is too large
  Too_Big : exception;

end Snappy;

