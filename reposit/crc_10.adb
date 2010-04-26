with Unchecked_Conversion;
with Bit_Ops;
package body Crc_10 is

  -- Char to byte
  type Byte is new Natural range 0 .. 255;
  for Byte'Size use Character'Size;
  function Char2Byte is new Unchecked_Conversion(Character, Byte);

  -- Reset computing of Crc
  procedure Rst (Crc : in out Crc_Type) is
  begin
    Crc.Bh := 0;
    Crc.Bl := 0;
  end Rst;

  -- Add Str to computed Crc
  procedure Add (Crc : in out Crc_Type; Str : in String) is
    use Bit_Ops;
    N : Integer;
  begin
    for I in Str'Range loop
      N := Integer(Char2Byte(Str(I)));
      Crc.Bl := Crc.Bl xor N;
      Crc.Bh := (Crc.Bh +  N) and 16#007F#;
    end loop;
  end Add;

  -- Get currently computed Crc (which is not reset)
  function Get (Crc : in Crc_Type) return Max_Crc_Range is
    Tbh, Tbl : Integer;
    Res : Integer;
    use Bit_Ops;
  begin
    -- lowest bit is same in Bh and Bl, reset in Bh
    Tbh := Shl (Crc.Bh and 16#7E#, 3);  -- Max  7f = ..1111110... = 127
    Tbl := Crc.Bl and 16#007F#;         -- Max  7f = .....1111111 = 127
    Res := Tbh xor Tbl;                 -- Max 3ff = 001111111111 = 1023
    return Max_Crc_Range (Res);
  end Get;

end Crc_10;

