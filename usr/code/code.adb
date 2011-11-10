with Ada.Characters.Latin_1;
with As.U, As.B, Sys_Calls, Argument, Upper_Str, Normal, My_Math, Text_Line,
     Basic_Proc;
with Grid_1, Grid_2, Vigenere;

procedure Code is
  Code : Boolean;
  Buff : As.U.Asu_Us;
  Rec : Grid_1.Coordinate_Rec;
  -- Better not allocate that in stack, but in heap.
  type Access_Long_String is access Grid_2.Long_String;
  Str  : constant Access_Long_String
       := new Grid_2.Long_String(1 .. My_Math.Inte(1_048_576));
  File_Too_Long : exception;


  Key_Len : Natural;
  Min_Key_Len : constant Natural := 8;
  Key : As.B.Asb_Bs(80);
  Key_Str : String (1 .. Key.Max);
  In_File  : Text_Line.File_Type;
  Out_File : Text_Line.File_Type;
  Sl : Vigenere.Long_Positive;
  Si : Vigenere.Long_Positive;
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
    for I in 1 .. Buff.Length loop
      begin
        Rec := Grid_1.Encode(Buff.Element(I));
      exception
        when Grid_1.Invalid_Character =>
          Basic_Proc.Put_Line_Output ("ERROR, invalid character.");
          Basic_Proc.Put_Line_Output (Buff.Image);
          for J in 1 .. I - 1 loop
            Basic_Proc.Put_Output ("-");
          end loop;
          Basic_Proc.Put_Line_Output ("^");
          raise;
      end;
      if Sl > Str'Last - 2 then
        raise File_Too_Long;
      end if;
      Str(Sl) := Rec.Row;
      Str(Sl + 1) := Rec.Col;
      Sl := Sl + 2;
    end loop;
  end Code_1;

  procedure Decode_1 is
  begin
    Buff.Set_Null;
    loop
      exit when Si > Sl - 1;
      Rec.Row := Str(Si);
      Rec.Col := Str(Si + 1);
      Si := Si + 2;
      C := Grid_1.Decode(Rec);
      Buff.Append (C);
      exit when C = Ada.Characters.Latin_1.Lf;
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
      Basic_Proc.Put_Line_Error ("Wrong argument. Usage : "
                     & Argument.Get_Parameter(Occurence => 0)
                     & " -c  |  -d     <input_file> [ <output_file> ] ");
      return;
  end;

  -- Get input file name
  Argument.Get_Parameter (Buff, Occurence => 2);
  -- Open input file
  begin
    In_File.Open_All (Text_Line.In_File, Buff.Image);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Unable to open input file >"
                               & Buff.Image & "<. Abort.");
      raise;
  end;

  -- Get output file name
  if Argument.Get_Nbre_Arg = 3 then
    Argument.Get_Parameter (Buff, Occurence => 3);
    -- Open output file
    begin
      Out_File.Create_All (Buff.Image);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Unable to create output file >"
                                 & Buff.Image & "<. Abort.");
        raise;
    end;
  else
    Out_File.Open_All (Text_Line.Out_File);
  end if;

  -- Get key
  loop
    Is_A_Tty := Echo(True);
    if Is_A_Tty then
      Basic_Proc.Put_Output ("Key: ");
      Is_A_Tty := Echo (False);
    end if;
    Basic_Proc.Get_Input (Key_Str, Key_Len);
    if Is_A_Tty then
      Is_A_Tty := Echo(True);
      Basic_Proc.New_Line_Output;
    end if;
    if Key_Len = 0 then
      Basic_Proc.Put_Line_Output ("Empty key.");
      if not Is_A_Tty then
        return;
      end if;
    elsif Code and then Key_Len < Min_Key_Len then
      Basic_Proc.Put_Line_Output ("Key too short ("
                    & Normal(Min_Key_Len, 1) & " min), try again.");
      if not Is_A_Tty then
        return;
      end if;
    else
      exit;
    end if;
  end loop;
  Key.Set (Key_Str (1 .. Key_Len));

  -- Initialize coding
  Grid_1.Initialize(Key.Image);

  if Code then
    -- Code through code 1
    Sl := 1;
    -- Code key
    Buff.Set (Key.Image & Ada.Characters.Latin_1.Lf);
    Code_1;
    -- Code input file
    loop
      -- Read input file
      Buff := In_File.Get;
      exit when Buff.Is_Null;
      -- Code line
      Code_1;
    end loop;
    Sl := Sl - 1;

    -- Code through code 2
    Str(1 .. Sl) := Grid_2.Encode (Key.Image, Str(1 .. Sl));

    -- Code through vigenere
    Vigenere.Encode (Key.Image, Str(1 .. Sl));

  else

    -- Read input file
    Sl := 1;
    loop
      Buff := In_File.Get;
      Text_Line.Trim (Buff);
      exit when Buff.Is_Null;
      -- Store characters
      for I in 1 .. Buff.Length loop
        Str(Sl) := Buff.Element (I);
        Sl := Sl + 1;
      end loop;
    end loop;
    Sl := Sl - 1;

    -- Decode through vigenere
    begin
      Vigenere.Decode (Key.Image, Str(1 .. Sl));
    exception
      when Vigenere.Decode_Error =>
        return;
    end;
    -- Decode through code 2
    Str(1 .. Sl) := Grid_2.Decode (Key.Image, Str(1 .. Sl));

  end if;

  if Code then
    -- Output result (cut each 80 cars)
    for I in 1 .. Sl loop
      Out_File.Put (Str(I) & "");
      if I mod 78 = 0 then
        Out_File.New_Line;
      end if;
    end loop;
    if Sl mod 78 /= 0 then
      Out_File.New_Line;
    end if;
  else
    -- Decode through code 1 and put
    begin
      Si := 1;
      Decode_1;
      Buff.Delete (Buff.Length, Buff.Length);
      if Buff.Image = Key.Image then
        -- Decode if key matches
        loop
          Decode_1;
          exit when Buff.Is_Null;
          Out_File.Put (Buff.Image);
        end loop;
      end if;
    exception
      when others =>
        null;
    end;
  end if;

  In_File.Close_All;
  Out_File.Close_All;
exception
  when File_Too_Long =>
    Basic_Proc.Put_Line_Error ("ERROR, input file too long.");
  when Grid_1.Invalid_Character =>
    -- Error while decoding
    null;
end Code;

