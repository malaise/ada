with Upper_Char, Lower_Char;
with Types, Io_Manager, Definition, Scrambler_Factory, Coder;
procedure Enigma is
  Byte : Io_Manager.Byte;
  Last_Char : constant Io_Manager.Byte := Character'Pos(Character'Last);
  Char, Uchar : Character;
  use type Io_Manager.Byte;
begin
  -- Init Scrambler factoy then Coder
  Scrambler_Factory.Init;
  Coder.Init;

  -- Skip initial offset
  begin
    Io_Manager.Skip_To (Definition.Read_Start_Byte);
  exception
    when Io_Manager.End_Error =>
      return;
  end;

  -- Read up to last offset
  Io_Manager.Set_Skip_From (Definition.Read_Last_Byte);

  -- Main loop
  loop
    -- Read next byte until end of flow
    begin
      Byte := Io_Manager.Read;
    exception
      when Io_Manager.End_Error =>
        Io_Manager.Flush;
        exit;
    end;
    -- Check if it can be encoded
    -- Must be a character, an upper or lower letter
    if Byte <= Last_Char then
      Char := Character'Val(Byte);
      if Char in Types.Letter then
        -- Encode if it is a letter
        Char := Coder.Encode (Char);
        Byte := Character'Pos(Char);
      else
        Uchar := Upper_Char (Char);
        if Uchar in Types.Letter then
          -- Encode if it is a letter
          Char := Lower_Char (Coder.Encode (Uchar));
          Byte := Character'Pos(Char);
        end if;
      end if;
    end if;
    -- Write byte
    Io_Manager.Write (Byte);
  end loop;

exception
  when Scrambler_Factory.Config_Error | Coder.Init_Failed =>
    Io_Manager.Set_Error_Exit_Code;
end Enigma;

