-- Compress or uncompress stdin to stdout
with Basic_Proc, Argument, Sys_Calls, Lzf;
procedure Azf is
  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        &  " [ -s <buffer_size> ] -c | -d | -h");
    Basic_Proc.Put_Line_Error (" -c : Compress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -d : Uncompress stdin to stdout");
    Basic_Proc.Put_Line_Error (" -s : Set buffer size in Mega bytes");
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

  -- Buffer size
  Buffer_Size : Positive := 1;

  -- Buffers and lengths
  Block_Size : constant := 1024 * 1024;
  type Buffer_Access is access Lzf.Byte_Array;
  Inb, Outb : Buffer_Access;
  Dummy, Inl, Outl : Natural;

  procedure Parse_Mode (I : Positive) is
  begin
    if Argument.Get_Parameter (I) ="-c" then
      Compress := True;
    elsif Argument.Get_Parameter (I) ="-d" then
      Compress := False;
    else
      Error ("Invalid argument");
      return;
    end if;
  end Parse_Mode;

  procedure Parse_Size (I : Positive) is
  begin
    Buffer_Size := Positive'Value (Argument.Get_Parameter (I + 1));
    if Buffer_Size > 1024 then
      Error ("Buffer size too large");
      return;
    end if;
  end Parse_Size;

begin

  -- Parse arguments
  if Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter (1) ="-h" then
      Help;
      Basic_Proc.Set_Error_Exit_Code;
      return;
    else
      Parse_Mode (1);
    end if;
  elsif  Argument.Get_Nbre_Arg = 3 then
    if Argument.Get_Parameter (1) = "-s" then
      Parse_Size (1);
      Parse_Mode (3);
    elsif Argument.Get_Parameter (2) = "-s" then
      Parse_Mode (1);
      Parse_Size (2);
    else
      Error ("Invalid argument");
      return;
    end if;
  else
    Error ("Invalid argument");
    return;
  end if;

  -- Create buffers
  Inb := new Lzf.Byte_Array(1 .. Buffer_Size * Block_Size);
  Outb := new Lzf.Byte_Array(1 .. Buffer_Size * Block_Size);

  -- Read input
  Inl := Sys_Calls.Read (Sys_Calls.Stdin, Inb.all'Address, Inb'Length);

  -- (Un)compress
  if Compress then
    Lzf.Compress (Inb(1 .. Inl), Outb.all, Outl);
  else
    Lzf.Uncompress (Inb(1..Inl), Outb.all, Outl);
  end if;

  -- Write output
  Dummy := Sys_Calls.Write (Sys_Calls.Stdout, Outb.all'Address, Outl);

exception
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Azf;

