with Ada.Text_Io, Ada.Exceptions;
with Text_Handler, Environ, Get_Line;
with Io_Manager;
separate (Scrambler_Factory)

-- Init scramblers from configuration file
procedure Init is
  -- Conf file name
  Default_Cnf : constant String := "enigma.cnf";
  Env_Name : constant String := "ENIGMA_CONF_FILE";
  File_Name : Text_Handler.Text (1024);

  -- Get_line
  package Get_Cnf is new Get_Line (26, 1, 80, "#");

  -- Report error in file
  procedure Error (Msg : in String) is
  begin
    Io_Manager.Put_Line_Error ("ERROR: " & Msg
     & " in file " & Text_Handler.Value (File_Name)
     & " at line " & Ada.Text_Io.Positive_Count'Image(Get_Cnf.Get_Line_No)
     & ".");
    raise Config_Error;
  end Error;

  -- Parse the scrambler definition
  procedure Parse (Sid : in Definition.Scrambler_Index; Str : in String) is
    Pair :  Types.Lid_Pair_T;
  begin

    -- Check global length
    if Str'Length /= Natural(Definition.Switch_Index'Last) then
      Error ("Invalid scrambler " & Sid'Img & " definition " & Str);
    end if;

    -- Check they are letters and unique
    for I in Str'Range loop
      if Str(I) not in Types.Letter then
        Error ("Invalid scrambler " & Sid'Img & " definition " & Str);
      end if;
      for J in Str'First .. I - 1 loop
        if Str(J) = Str(I) then
          Error ("Letter " & Str(J) & " used twice in scrambler "
               & Sid'Img & " definition");
        end if;
      end loop;
    end loop;

    -- Feed scrambler, init with identity
    Scramblers(Sid).Scrambler := Create;
    for I in Str'Range loop
      -- Pair is Index, GotChar
      Pair.E := Types.Lid(I - Str'First);
      Pair.D :=  Types.Id_Of (Str(I));
      Set (Scramblers(Sid).Scrambler, Pair);
    end loop;
    Scramblers(Sid).Defined := True;
    Scramblers(Sid).Used := False;
  end Parse;

  -- Data expected fomr file: 0 => Scrambler id, 1 => ref, 2 => assoc
  type Expect_Range is mod 3;
  Expected : Expect_Range := 0;
  -- Scrambler id
  Sid : Definition.Scrambler_Index;
  -- Second line of a scrambler def
  Ref_Line : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
begin
  -- Initialize Scramblers
  for I in Scramblers'Range loop
    Scramblers(I).Defined := False;
    Scramblers(I).Used := False;
  end loop;
    
  -- Check if env variable overwrites default file
  Environ.Get_Txt (Env_Name, File_Name);
  if Text_Handler.Empty (File_Name) then
    Text_Handler.Set (File_Name, Default_Cnf);
  end if;
  -- Open file 
  begin
    Get_Cnf.Open (Text_Handler.Value (File_Name));
  exception
    when Ada.Text_Io.Name_Error =>
      Io_Manager.Put_Line_Error ("ERROR: Configuration file "
       & Text_Handler.Value (File_Name) & " not found.");
      Io_Manager.Put_Line_Error ("Perhaps setting env "
       & Env_Name & " might help.");
      raise Config_Error;
    when Error:others =>
      Io_Manager.Put_Line_Error ("ERROR: "
       & Ada.Exceptions.Exception_Name(Error)
       & " opening configuration file "
       & Text_Handler.Value (File_Name) & ".");
      raise Config_Error;
  end;

  -- Parse file
  loop
    -- Force parsing of line: one word
    begin
      if Get_Cnf.Get_Word_Number /= 1 then
        raise Get_Cnf.Too_Many_Words;
      end if;
    exception
      when Get_Cnf.Too_Many_Words =>
        Error ("Too many words");
    end;
    case Expected is
      when 0 =>
        -- Expect a Sid
        begin
          Sid := Definition.Scrambler_Index'Value (Get_Cnf.Get_First_Word);
        exception
          when Constraint_Error =>
            Error ("Invalid scrambler number");
        end;
        -- Must not be already defined
        if Scramblers(Sid).Defined then
          Error ("Scrambler number " & Sid'Img & " already defined");
        end if;
      when 1 =>
        -- Expect a reference line
        if Get_Cnf.Get_First_Word /= Ref_Line then
          Error ("Invalid reference line");
        end if;
      when 2 =>
        -- Expect a mapping
        Parse (Sid, Get_Cnf.Get_First_Word);
    end case;
    -- Next
    Expected := Expected + 1;
    Get_Cnf.Read_Next_Line;
  end loop;  
exception
  when Get_Cnf.No_More_Line =>
    Get_Cnf.Close;
    if Expected /= 0 then
      -- Eof within a scrambler definition
      Error ("unexpected end of file");
    end if;
end Init;

