with Unchecked_Conversion;
with Normal, Bit_Ops;
with Parse, Data_Base;
package body Crc is

  type Byte is new Natural range 0 .. 255;
  for Byte'Size use Character'Size;

  function Char2Byte is new Unchecked_Conversion(Character, Byte);

  function Get return Crc_Str is
    Bh, Bl : Integer;
    use Bit_Ops;
  
    procedure Add (B : in Byte) is
    begin
      Bl := Bl xor Integer(B);
      Bh := (Bh +  Integer(B)) and 16#007F#;
    end Add;

    Item : Data_Base.Item_Rec;
    Int : Integer;

    use type Data_Base.Item_Rec;
  begin
    Bh := 0;
    Bl := 0;
    Data_Base.Read_First (Item);
    loop
      exit when Item = Data_Base.No_Item;
      for I in Parse(Item.Name)'Range loop
        Add (Char2Byte (Item.Name(I)));
      end loop;
      for I in 1 .. Item.Data_Len loop
        Add (Char2Byte (Item.Data(I)));
      end loop;
      Data_Base.Read_Next (Item);
    end loop;
    -- lowest bit is same in BH and BL, reset in BH
    Bh := Shl (Bh and 16#7E#, 3);  -- Max  7F = ..1111110... = 127
    Bl := Bl and 16#007F#;         -- Max  7F = .....1111111 = 127
    Int := Bh xor Bl;              -- Max 3FF = 001111111111 = 1023
    return Normal (Int, Crc_Str'Length, Gap => '0');

  end Get;

end Crc;


