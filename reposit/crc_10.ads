package Crc_10 is

  -- Maximum value of Crc : 1023, due to the implemented function
  Max_Crc_Value : constant := 16#3FF#;
  type Max_Crc_Range is new Integer range 0 .. Max_Crc_Value;

  -- Reset computing of Crc
  procedure Rst;

  -- Add Str to computed Crc
  procedure Add (Str : String);

  -- Get currently computed Crc (which is not reset)
  function Get return Max_Crc_Range;

end Crc_10;

