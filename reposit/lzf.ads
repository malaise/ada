with C_Types;
package Lzf is

  -- An array of bytes to (un)compress
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;

  -- Compress Input into Output
  -- Outlen is less than 104% of Input length
  procedure Compress (Input : in Byte_Array;
                      Output : out Byte_Array;
                      Outlen : out Natural);

  -- Uncompress Input into Output
  -- May raise Too_Big if Output is too small
  procedure Uncompress (Input : in Byte_Array;
                        Output : out Byte_Array;
                        Outlen : out Natural);

end Lzf;

