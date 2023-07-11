-- Lz4 compression
with C_Types;
package Lz4 is

  -- An array of bytes to (un)compress
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;

  -- Compress Input into Output
  -- Outlen is at most an extra 8 MBbs for 2 GB (0.4%)
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

end Lz4;

