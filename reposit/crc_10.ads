package Crc_10 is
  type Crc_Type is tagged private;

  -- Maximum value of Crc : 1023, due to the implemented function
  Max_Crc_Value : constant := 16#3FF#;
  type Max_Crc_Range is new Integer range 0 .. Max_Crc_Value;

  -- Reset computing of Crc
  procedure Rst (Crc : in out Crc_Type);

  -- Add Str to computed Crc
  procedure Add (Crc : in out Crc_Type; Str : in String);

  -- Get currently computed Crc (which is not reset)
  function Get (Crc : in Crc_Type) return Max_Crc_Range;

private
  type Crc_Type is tagged record
    Bh, Bl : Integer := 0;
  end record;

end Crc_10;

