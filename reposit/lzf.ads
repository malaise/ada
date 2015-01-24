with C_Types;
-- Lzf compression
package Lzf is

  -- An array of bytes to (un)compress
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;

  -- Speed of compression
  type Compression_Speed is (Fast, Very_Fast, Ultra_Fast);
  Default_Speed : constant Compression_Speed := Very_Fast;

  -- Compress Input into Output
  -- Outlen is at most one extra byte each 31 bytes, 3.3% overhead
  -- May raise Too_Big if Output is too small
  procedure Compress (Input  : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Natural;
                      Speed  : in Compression_Speed := Default_Speed);

  -- Uncompress Input into Output
  -- May raise Too_Big if Output is too small
  procedure Uncompress (Input  : in Byte_Array;
                        Output : out Byte_Array;
                        Outlen : out Natural);

  -- Output is too small
  Too_Big : exception;

end Lzf;

