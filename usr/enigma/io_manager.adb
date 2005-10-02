with Sys_Calls;
package body Io_Manager is

  -- Skip the Nb_Bytes of input flow
  procedure Skip (Nb_Bytes : Positive) is
    B : Byte;
  begin
    for I in 1 .. Nb_Bytes loop
      B := Read;
    end loop;
  end Skip;

  -- Read next byte
  function Read return Byte is
    N : Natural;
    B : Byte;
  begin
    N := Sys_Calls.Read (Sys_Calls.Stdin, B'Address, 1);
    return B;
  exception
    when Sys_Calls.System_Error =>
      -- End of input flow
      raise End_Error;
  end Read;

  -- Write byte
  procedure Write (B : in Byte) is
    N : Natural;
  begin
    N := Sys_Calls.Write (Sys_Calls.Stdout, B'Address, 1);
  end Write;

  procedure Put_Error      (Str : in String) renames Sys_Calls.Put_Error;
  procedure Put_Line_Error (Str : in String) renames Sys_Calls.Put_Line_Error;
  procedure New_Line_Error                   renames Sys_Calls.New_Line_Error;

  procedure Set_Error_Exit_Code renames Sys_Calls.Set_Error_Exit_Code;
end Io_Manager;

