-- Parses command line and returns enigma definition
with Argument;
with Io_Manager;
package body Definition is

  Parsed_Rec : Def_Rec;
  Start_Byte : Positive := 1;
  Last_Byte : Natural := 0;
  Rec_Parsed : Boolean := False;
  procedure Parse;

  -- Parse args and fill Def
  procedure Read_Definition (Def : out Def_Rec) is
  begin
    if not Rec_Parsed then
      Parse;
    end if;
    Rec_Parsed := True;
    Def := Parsed_Rec;
  end Read_Definition;

  -- Get start byte offset
  function Read_Start_Byte return Positive is
  begin
    if not Rec_Parsed then
      Parse;
    end if;
    Rec_Parsed := True;
    return Start_Byte;
  end Read_Start_Byte;

  -- Get last byte offset (0 if none)
  function Read_Last_Byte return Natural is
  begin
    if not Rec_Parsed then
      Parse;
    end if;
    Rec_Parsed := True;
    return Last_Byte;
  end Read_Last_Byte;


  -------------------------------------------------------------------------
  Switch_Key : constant String := "s";
  Jammers_Key : constant String := "j";
  Back_Key : constant String := "b";
  Start_Key : constant String := "f";
  Last_Key : constant String := "l";

  procedure Error (Msg : in String) is
    procedure Ple (S : in String) renames Io_Manager.Put_Line_Error;
  begin
    Ple ("ERROR: " & Msg & ".");
    Io_Manager.New_Line_Error;
    Ple ("Usage: " & Argument.Get_Program_Name
       & " [ <switch> ] [ <jammers>  ] <back> [ <first_index> ] [ <last_index> ]");
    Ple ("   <switch>        ::= -s<switch_def>");
    Ple ("   <switch_def>    ::= { <upperletter><upperletter> }");
    Ple ("   <jammers>       ::= -j{ <scrambler_num><upperletter><upperletter> }");
    Ple ("   <back>          ::= -b<scrambler_num><upperletter>");
    Ple ("   <first_index>   ::= -f<positive>       (default 1)");
    Ple ("   <last_index>    ::= -l<positive>       (default none)");
    Ple ("   <scrambler_num> ::= 1 .. 9");
    Ple ("   <upperletter>   ::= A .. Z");
    Ple ("Up to 8 jammers can be defined and one back must be defined.");
    Ple ("They must all have different numbers.");
    raise Invalid_Definition;
  end Error;

  -- Check that Key is one of the expected keys
  procedure Is_Valid_Key (Key : in String) is
  begin
   if Key'Length =0 then
     Error ("Invalid argument -");
   end if;
   if       Key (1 .. 1) /= Jammers_Key
   and then Key (1 .. 1) /= Back_Key
   and then Key (1 .. 1) /= Start_Key
   and then Key (1 .. 1) /= Last_Key
   and then Key (1 .. 1) /= Switch_Key then
     Error ("Invalid argument -" & Key);
   end if;
  end Is_Valid_Key;

  procedure Check_Twice (Key : in String; Msg : in String) is
    use Argument;
  begin
    if Get_Position (2, Key) /= 0 then
      Error (Msg);
    end if;
  exception
    when Argument_Not_Found =>
      null;
  end Check_Twice;

  procedure Check is
    use Argument;
  begin
    -- All must be keys
    begin
      declare
        Not_Key_Arg : constant String := Get_Parameter (1, Not_Key);
      begin
        Error ("Unexpected argument: " & Not_Key_Arg);
    end;
    exception
      when Argument_Not_Found =>
        -- Normal
        null;
    end;
    -- Check all keys are within the expected keys
    for I in 1 .. Get_Nbre_Arg loop
      Is_Valid_Key (Get_Parameter (I, Any_Key));
    end loop;
    -- Check all key is unique
    Check_Twice (Switch_Key, "Switch defined twice");
    Check_Twice (Jammers_Key, "Jammers defined twice");
    Check_Twice (Back_Key, "Back defined twice");
    Check_Twice (Start_Key, "First offset defined twice");
    Check_Twice (Last_Key, "Last offset defined twice");
  end Check;

  -- Parse a switch: N letters
  procedure Parse_Switch (Str : in String; Switch : out Switch_Definition) is
    Index : Switch_Index;
  begin
    -- Must be non empty pairs of letter, max 26 pairs
    if Str'Length = 0
    or else Str'Length rem 2 /= 0
    or else Str'Length > Natural(Switch_Range'Last) * 2 then
      Error ("Invalid number of letters in switch definition " & Str);
    end if;
    -- Init size of switch definition
    Switch := (Str'Length / 2, (others => (Types.Letter'First,
                                           Types.Letter'First)) );

    -- Store letters
    for I in Str'Range loop
      -- Check they are letters
      if Str(I) not in Types.Letter then
        Error ("Invalid letter " & Str(I) & " in switch definition " & Str);
      end if;
      if I rem 2 = 1 then
        Index := Switch_Index (I / 2 + 1);
        -- Encoding part: check unicity and store
        for J in 1 .. Index - 1 loop
          if Switch.Switch(J).E = Str(I) then
            Error ("Dual first letter " & Str(I) & " in switch definition "
                   & Str);
          end if;
        end loop;
        Switch.Switch(Index).E := Str(I);
      else
        -- Decoding part: check unicity and store
        for J in 1 .. Index - 1 loop
          if Switch.Switch(J).D = Str(I) then
            Error ("Dual second letter " & Str(I) & " in switch definition "
                   & Str);
          end if;
        end loop;
        Switch.Switch(Index).D := Str(I);
      end if;
    end loop;
  end Parse_Switch;

  -- Parse the back: a number and a letter
  procedure Parse_Back (Name : in String; Str : in String;
                        Back : out Back_Definition) is
  begin
    -- A number and a letter
    if Str'Length /= 2 then
      Error ("Invalid back definition " & Str & " for " & Name);
    end if;
    -- Scrambler number
    begin
      Back.Scrambler := Scrambler_Index'Value(
                              Str(Str'First .. Str'First));
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler number " & Str(Str'First) & " for " & Name);
    end;
    -- Scrambler offset
    declare
    begin
      Back.Offset := Str(Str'Last);
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler offset " & Str(Str'Last) & " for " & Name);
    end;
  end Parse_Back;

  -- Parse a jammer: a number and two letters
  procedure Parse_Jammer (Name : in String; Str : in String;
                          Jammer : out Jammer_Definition) is
  begin
    -- A number and two letters
    if Str'Length /= 3 then
      Error ("Invalid jammer definition " & Str & " for " & Name);
    end if;
    -- Scrambler number
    begin
      Jammer.Scrambler := Scrambler_Index'Value(
                              Str(Str'First .. Str'First));
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler number " & Str(Str'First) & " for " & Name);
    end;
    -- Scrambler offset
    declare
      Index : constant Integer := Integer'Succ(Str'First);
    begin
      Jammer.Offset := Str(Index);
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler offset " & Str(Index) & " for " & Name);
    end;
    -- Scrambler carry offset
    begin
      Jammer.Carry_Offset := Str(Str'Last);
    exception
      when Constraint_Error =>
        Error ("Invalid jammer carry " & Str(Str'Last) & " for " & Name);
    end;
  end Parse_Jammer;

  procedure Parse is
    use Argument;
  begin
    Check;
    -- Parse start index
    begin
      Start_Byte := Positive'Value(Get_Parameter (1, Start_Key));
    exception
      when Argument_Not_Found =>
        null;
      when others =>
        Error ("Invalid first index: " & Get_Parameter (1, Start_Key));
    end;
    -- Parse last index
    begin
      Last_Byte := Natural'Value(Get_Parameter (1, Last_Key));
      if Last_Byte = 0 then
        raise Constraint_Error;
      end if;
    exception
      when Argument_Not_Found =>
        -- Last byte remains 0
        null;
      when others =>
        Error ("Invalid last index: " & Get_Parameter (1, Last_Key));
    end;
    -- Parse switch
    begin
      Parse_Switch (Get_Parameter (1, Switch_Key), Parsed_Rec.Switch);
    exception
      when Argument_Not_Found =>
        null;
    end;
    -- Parse back
    begin
      Parse_Back ("back", Get_Parameter (1, Back_Key), Parsed_Rec.Back);
    exception
      when Argument_Not_Found =>
        Error ("No back defined");
    end;
    -- Parse jammers
    begin
      declare
        Str : constant String :=  Get_Parameter (1, Jammers_Key);
        Index : Jammers_Index;
      begin
        -- Check positive and number of chars rem 3
        --  (triplets of Num, Offset, Carry)
        if Str'Length < 3 or else Str'Length rem 3 /= 0 then
          Error ("Invalid jammers definition " & Str);
        end if;
        -- Check max number of jammers
        if Str'Length / 3 > Natural(Jammers_Range'Last) then
          Error ("To many jammers defined in " & Str);
        end if;
        Parsed_Rec.Jammers := (Str'Length / 3,
                               (others => (Scrambler_Index'First,
                                          Types.Letter'First,
                                          Types.Letter'First)) );
        for I in 1 .. Str'Length / 3 loop
          Index := Jammers_Index(I);
          Parse_Jammer ("jammer" & I'Img,
                         Str(I * 3 - 2 .. I * 3),
                         Parsed_Rec.Jammers.Jammers(Index) );
          -- Check unicity
        end loop;
      end;
    exception
      when Argument_Not_Found =>
        null;
    end;
  end Parse;

end Definition;

