package Crc is

  subtype Crc_Str is String (1 .. 4);
  Dummy_Crc : constant Crc_Str := "  -1";

  function Get return Crc_Str;

end Crc;

