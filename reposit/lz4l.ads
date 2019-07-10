with C_Types, Long_Longs;
-- Lz4-like compression of large buffer.
-- The algorithm is quite similar to lz4, but enhanced to support large
--  reference offset (see implementation details in the body).
-- As a consequence, the compressed format is not compatible with lz4.
package Lz4L is

  -- A (large) array of bytes to (un)compress
  subtype Ll_Natural is Long_Longs.Ll_Natural;
  subtype Ll_Positive is Long_Longs.Ll_Positive;
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Ll_Positive range <>) of Byte;

  -- Compress Input into Output
  -- Outlen is at most an extra 80 MBbs for 2 GB (4%)
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

