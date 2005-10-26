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

  -- Buffer of bytes
  type Real_Byte is new Byte;
  for Real_Byte'Size use 1 * System.Storage_Unit;
  Buffer_Size : constant := 1024;
  subtype Index_Range is Natural range 0 .. Buffer_Size;
  subtype Buffer_Range is Index_Range range 1 .. Index_Range'Last;
  type Buffer_Array is array (Buffer_Range) of Real_Byte;

  -- Input buffer and last valid byte of it
  Input_Buffer : Buffer_Array;
  Last_Input : Buffer_Range := 1;
  -- Current index in input buffer
  -- This initial value will force initial reading of buffer
  Input_Index : Buffer_Range := Last_Input;

  -- Read next byte
  function Read return Byte is
    Result : Natural;
  begin
    -- Check current offset <= Last_Byte
    Current_Offset := Current_Offset + 1;
    if Last_Byte /= 0 and then Current_Offset > Last_Byte then
      raise End_Error;
    end if;
    -- Check if need to read buffer
    if Input_Index = Last_Input then
      Result := Sys_Calls.Read (Sys_Calls.Stdin,
                                Input_Buffer(Input_Buffer'First)'Address,
                                Buffer_Size);
      if Result <= 0 then
        raise End_Error;
      end if;
      Last_Input := Result;
      Input_Index := 1;
    else
      Input_Index := Input_Index + 1;
    end if;
    -- Get byte from buffer
    return Byte(Input_Buffer(Input_Index));
  exception
    when Sys_Calls.System_Error =>
      -- End of input flow
      raise End_Error;
  end Read;

  -- Output buffer and index of last writen byte of it
  Output_Buffer : Buffer_Array;
  Last_Output : Index_Range := 0;

  -- Write byte
  procedure Write (B : in Byte) is
  begin
    if Last_Output = Output_Buffer'Last then
      -- Buffer is full, flush it
      Flush;
      Last_Output := 0;
    end if;
    -- Store Byte in buffer
    Last_Output := Last_Output + 1;
    Output_Buffer(Last_Output) := Real_Byte(B);
  end Write;

  procedure Flush is
    N : Natural;
  begin
    -- Write buffer on stdout
    if Last_Output /= 0 then
      N := Sys_Calls.Write (Sys_Calls.Stdout,
                            Output_Buffer'Address,
                            Last_Output);
    end if;
  end Flush;

  procedure Put_Error      (Str : in String) renames Sys_Calls.Put_Error;
  procedure Put_Line_Error (Str : in String) renames Sys_Calls.Put_Line_Error;
  procedure New_Line_Error                   renames Sys_Calls.New_Line_Error;

  procedure Set_Error_Exit_Code renames Sys_Calls.Set_Error_Exit_Code;
end Io_Manager;

