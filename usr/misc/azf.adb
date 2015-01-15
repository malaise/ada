-- Compress or uncompress stdin to stdout
with Basic_Proc, Argument, Sys_Calls, Lzf;
procedure Azf is
  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        &  " -c | -d | -h");
    Basic_Proc.Put_Line_Error (" -c : Compress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -d : Uncompress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -h : Display this help");
  end Help;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Help;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Do we compress
  Compress : Boolean;

  -- Buffers and lengths
  Block_Size : constant := 1024 * 1024 * 1024;
  Inb, Outb : Lzf.Byte_Array (1 .. Block_Size);
  Dummy, Inl, Outl : Natural;

begin

  -- Parse arguments
  if Argument.Get_Nbre_Arg /= 1 then
    Error ("Invalid argument");
    return;
  end if;
  if Argument.Get_Parameter (1) ="-c" then
    Compress := True;
  elsif Argument.Get_Parameter (1) ="-d" then
    Compress := False;
  elsif Argument.Get_Parameter (1) ="-h" then
    Help;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  else
    Error ("Invalid argument");
    return;
  end if;

  -- Read input
  Inl := Sys_Calls.Read (Sys_Calls.Stdin, Inb'Address, Inb'Length);

  -- (Un)compress
  if Compress then
    Lzf.Compress (Inb(1 .. Inl), Outb, Outl);
  else
    Lzf.Uncompress (Inb(1..Inl), Outb, Outl);
  end if;

  -- Write output
  Dummy := Sys_Calls.Write (Sys_Calls.Stdout, Outb'Address, Outl);

exception
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Azf;

