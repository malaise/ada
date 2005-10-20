with System;
with Sys_Calls;
package body Io_Manager is

  Current_Offset : Natural := 0;
  Last_Byte : Natural := 0;

  -- Skip to Bytes_Offset
  procedure Skip_To (Bytes_Offset : Positive) is
    B : Byte;
  begin
    for I in 1 .. Bytes_Offset - 1 loop
      B := Read;
    end loop;
  end Skip_To;

  -- Set last offset to read up to
  procedure Set_Skip_From (Bytes_Offset : Natural) is
  begin
    Last_Byte := Bytes_Offset;
  end Set_Skip_From;

  type Real_Byte is new Byte;
  for Real_Byte'Size use 1 * System.Storage_Unit;
  -- Read next byte
  function Read return Byte is
    N : Natural;
    B : Real_Byte;
  begin
    Current_Offset := Current_Offset + 1;
    if Last_Byte /= 0 and then Current_Offset > Last_Byte then
      raise End_Error;
    end if;
    N := Sys_Calls.Read (Sys_Calls.Stdin, B'Address, 1);
    if N = 1 then
      return Byte(B);
    else
      raise End_Error;
    end if;
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

