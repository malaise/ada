package Io_Manager is

  type Byte is new Natural range 0 .. 255;

  -- Skip the Nb_Bytes of input flow
  procedure Skip (Nb_Bytes : Positive);

  -- Read next byte of input flow
  End_Error : exception;
  function Read return Byte;

  -- Write byte on output flow
  procedure Write (B : in Byte);

  -- Put error
  procedure Put_Error (Str : in String);
  procedure Put_Line_Error (Str : in String);
  procedure New_Line_Error;

  -- Set error exit code
  procedure Set_Error_Exit_Code;
end Io_Manager;

