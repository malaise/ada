with Ada.Text_Io, Ada.Characters.Latin_1;
with Sys_Calls, Text_Handler, My_Io, Argument, Upper_Str, Normal, My_Math;
with Grid_1, Grid_2;

procedure Code is
  Code : Boolean;
  Max_Line_Len : constant := 1024;
  Buff : String (1 .. Max_Line_Len);
  Max_File_Len : constant := 58;
  Rec : Grid_1.Coordinate_Rec;
  -- Better not allocate that in stack, but in heap.
  type Access_Long_String is access Grid_2.Long_String;
  Str  : Access_Long_String
       := new Grid_2.Long_String(1 .. My_Math.Inte(1_048_576));
  File_Too_Long : exception;


  subtype Line_Index is Natural range 0 .. Max_Line_Len;
  Len : Line_Index;
  Min_Key_Len : constant Line_Index := 8;
  Line_Too_Long : exception;
  Key : Text_Handler.Text(80);
  In_File  : Ada.Text_Io.File_Type;
  Out_File : Ada.Text_Io.File_Type;
  Sl : Grid_2.Long_Positive;
  Si : Grid_2.Long_Positive;
  C : Character;
  Is_A_Tty : Boolean;

  function Echo (On : in Boolean) return Boolean is
  begin
    if On then
      return Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Canonical);
    else
      return Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.No_Echo);
    end if;
  end Echo;

  procedure Code_1 is
  begin
    begin
      Len := Len + 1;
    exception
      when Constraint_Error =>
        raise Line_Too_Long;
    end;
    Buff(Len) := Ada.Characters.Latin_1.Cr;
    for I in 1 .. Len loop
      begin
        Rec := Grid_1.Encode(Buff(I));
      exception
        when Grid_1.Invalid_Character =>
          Ada.Text_Io.Put_Line ("ERROR, invalid character.");
          Ada.Text_Io.Put_Line (Buff(1 .. Len));
          raise;
      end;
      if Sl > Str'Last then
        raise File_Too_Long;
      end if;
      Str(Sl) := Rec.Row;
      Str(Sl + 1) := Rec.Col;
      Sl := Sl + 2;
    end loop;
  end Code_1;

  procedure Decode_1 is
  begin
    Len := 0;
    loop
      Rec.Row := Str(Si);
      Rec.Col := Str(Si + 1);
      Si := Si + 2;
      C := Grid_1.Decode(Rec);
      Len := Len + 1;
      Buff(Len) := C;
      exit when C = Ada.Characters.Latin_1.Cr;
    end loop;
  end Decode_1;

begin

  -- Get coding mode
  begin
    if Argument.Get_Nbre_Arg < 2 or else Argument.Get_Nbre_Arg > 3 then
      raise Argument.Argument_Not_Found;
    end if;
    if Upper_Str (Argument.Get_Parameter) = "-C" then
      Code := True;
    elsif Upper_Str (Argument.Get_Parameter) = "-D" then
      Code := False;
    else
      raise Argument.Argument_Not_Found;
    end if;
  exception
    when others =>
      My_Io.Put_Line ("Wrong argument. Usage : "
                     & Argument.Get_Parameter(Occurence => 0) 
                     & " -c  |  -d     <input_file> [ <output_file> ] ");
      return;
  end;

  -- Get input file name
  Argument.Get_Parameter (Buff(1 .. Max_File_Len), Len, Occurence => 2);
  -- Open input file
  begin
    Ada.Text_Io.Open (In_File, Ada.Text_Io.In_File, Buff(1 .. Len));
  exception
    when others =>
      My_Io.Put_Line ("Unable to open input file >"
                     & Buff (1 .. Len) & "<. Abort.");
      raise;
  end;

  -- Get output file name
  if Argument.Get_Nbre_Arg = 3 then
    Argument.Get_Parameter (Buff(1 .. Max_File_Len), Len, Occurence => 3);
    -- Open output file
    begin
      begin
        Ada.Text_Io.Open (Out_File, Ada.Text_Io.Out_File, Buff(1 .. Len));
      exception
        when Ada.Text_Io.Name_Error =>
          Ada.Text_Io.Create (Out_File, Ada.Text_Io.Out_File, Buff(1 .. Len));
      end;
    exception
      when others =>
        My_Io.Put_Line ("Unable to open/create output file >"
                      & Buff (1 .. Len) & "<. Abort.");
        raise;
    end;
  end if;

  -- Get key
  loop
    Is_A_Tty := Echo(True);
    if Is_A_Tty then
      My_Io.Put ("Key: ");
      Is_A_Tty := Echo (False);
    end if;
    My_Io.Get_Line (Buff, Len);
    if Is_A_Tty then
      Is_A_Tty := Echo(True);
      My_Io.New_Line;
    end if;
    if Len = 0 then
      My_Io.Put_Line ("Program aborted by user.");
      return;
    elsif Code and then Len < Min_Key_Len then
      My_Io.Put_Line ("Too short ("
                    & Normal(Min_Key_Len, 1) & " min), try again.");
      if not Is_A_Tty then
        return;
      end if;
    else
      exit;
    end if;
  end loop;

  if Argument.Get_Nbre_Arg = 3 then
    Ada.Text_Io.Set_Output (Out_File);
  end if;

  Text_Handler.Set (Key, Buff (1 .. Len));

  -- Initialize coding
  Grid_1.Initialize(Text_Handler.Value(Key));

  if Code then
    Sl := 1;
    -- Code key
    Code_1;
    -- Code input file
    loop
      -- Read input file
      begin
        Ada.Text_Io.Get_Line (In_File, Buff, Len);
      exception
        when Ada.Text_Io.End_Error => exit;
      end;
      -- Code line
      Code_1;
    end loop;
    Sl := Sl - 1;

    -- Code through code 2
    Str (1 .. Sl) := Grid_2.Encode(Text_Handler.Value(Key), Str(1 .. Sl));
    -- Output result (cut each 80 cars)
    for I in 1 .. Sl loop
      Ada.Text_Io.Put (Str(I));
      if I mod 78 = 0 then
        Ada.Text_Io.New_Line;
      end if;
    end loop;
    if Sl mod 78 /= 0 then
      Ada.Text_Io.New_Line;
    end if;
  else
    -- Decode input file
    Sl := 1;
    loop
      -- Read input file
      begin
        Ada.Text_Io.Get_Line (In_File, Buff, Len);
      exception
        when Ada.Text_Io.End_Error => exit;
      end;
      -- Store characters
      for I in 1 .. Len loop
        Str(Sl) := Buff(I);
        Sl := Sl + 1;
      end loop;
    end loop;
    Sl := Sl - 1;

    -- Decode through code 2
    Str (1 .. Sl) := Grid_2.Decode(Text_Handler.Value(Key), Str(1 .. Sl));

    -- Decode through code 1
    begin
      Si := 1;
      Decode_1;
      if Buff(1 .. Len-1) = Text_Handler.Value (Key) then
        loop
          Decode_1;
          exit when Len = 0;
          Ada.Text_Io.Put_Line (Buff(1 .. Len-1));
        end loop;
      end if;
    exception
      when others =>
        null;
    end;

  end if;


  Ada.Text_Io.Close (In_File);
  Ada.Text_Io.Set_Output (Ada.Text_Io.Standard_Output);
  if Ada.Text_Io.Is_Open (Out_File) then
    Ada.Text_Io.Close (Out_File);
  end if;
exception
  when Line_Too_Long =>
    Ada.Text_Io.Put_Line ("ERROR, input line too long.");
  when File_Too_Long =>
    Ada.Text_Io.Put_Line ("ERROR, input file too long.");
  when Grid_1.Invalid_Character =>
    null;
end Code;

