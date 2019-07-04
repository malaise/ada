with C_Types, Long_Longs;
-- Lz4-like compression of large buffer
-- The algorithm is similar but enhanced to support large reference offset, so
--  the compressed format is not compatible with lz4
package Lz4L is

  -- An array of bytes to (un)compress
  subtype Ll_Natural is Long_Longs.Ll_Natural;
  subtype Ll_Positive is Long_Longs.Ll_Positive;
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Ll_Positive range <>) of Byte;

  -- Compress Input into Output
  -- Outlen is at most an extra 8 MBbs for 2 GB (0.4%)
  -- May raise Too_Big if Output is too small
  procedure Compress (Input  : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Ll_Natural);

  -- Uncompress Input into Output
  -- May raise Too_Big if Output is too small
  procedure Uncompress (Input  : in Byte_Array;
                        Output : out Byte_Array;
                        Outlen : out Ll_Natural);

  -- Output is too small or an offset is too large
  Too_Big : exception;

end Lz4L;

