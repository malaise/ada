package Io_Manager is

  type Byte is new Natural range 0 .. 255;

  -- Skip to Bytes_Offset of input flow
  procedure Skip_To (Bytes_Offset : Positive);

  -- Set last offset to read up to
  procedure Set_Skip_From (Bytes_Offset : Natural);

  -- Read next byte of input flow
  End_Error : exception;
  function Read return Byte;

  -- Write byte on output flow
  procedure Write (B : in Byte);
  procedure Flush;

  -- Put error
  procedure Put_Error (Str : in String);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;

  -- Set error exit code
  procedure Set_Error_Exit_Code;
end Io_Manager;

