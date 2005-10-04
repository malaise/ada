-- Parses command line and returns enigma definition
with Argument;
with Io_Manager;
package body Definition is

  Parsed_Rec : Def_Rec;
  Start_Byte : Positive := 1;
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

  -------------------------------------------------------------------------
  First_Switch_Key : constant String := "fs";
  Jammers_Key : constant String := "j";
  Back_Key : constant String := "b";
  Last_Switch_Key : constant String := "ls";
  Start_Key : constant String := "s";

  procedure Error (Msg : in String) is
    procedure Ple (S : in String) renames Io_Manager.Put_Line_Error;
  begin
    Ple ("ERROR: " & Msg & ".");
    Io_Manager.New_Line_Error;
    Ple ("Usage: " & Argument.Get_Program_Name
       & " [ <first_switch> ] [ <jammers>  ] <back>");
    Ple ("              [ <last_switch> ] [ <start_index> ]");
    Ple ("   <first_switch>  ::= -fs<switch>");
    Ple ("   <last_switch>   ::= -ls<switch>");
    Ple ("   <switch>        ::= { <upperletter><upperletter> }");
    Ple ("   <jammers>       ::= -j{ <scrambler> }");
    Ple ("   <scrambler>     ::= <scrambler_num><upperletter>");
    Ple ("   <back>          ::= -b<scrambler>");
    Ple ("   <start_index>   ::= -s<positive>       (default 1)");
    Ple ("   <scrambler_num> ::= 1 .. 9");
    Ple ("   <upperletter>   ::= A .. Z");
    Ple ("Up to 8 jammers can be defined and one back must be defined.");
    Ple ("They must all have different numbers.");
    raise Invalid_Definition;
  end Error;

  -- Check that Key is one of the expected keys
  procedure Is_Valid_Key (Key : in String) is
  begin
   if Key'Length < 2 then
     Error ("Invalid argument -" & Key);
   end if;
   if       Key (1 .. 1) /= Jammers_Key
   and then Key (1 .. 1) /= Back_Key
   and then Key (1 .. 1) /= Start_Key
   and then Key (1 .. 2) /= First_Switch_Key
   and then Key (1 .. 2) /= Last_Switch_Key then
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
    Check_Twice (First_Switch_Key, "First switch defined twice");
    Check_Twice (Last_Switch_Key, "Last switch defined twice");
    Check_Twice (Jammers_Key, "Jammers defined twice");
    Check_Twice (Back_Key, "Back defined twice");
    Check_Twice (Start_Key, "Start offset defined twice");
  end Check;

  -- Parse a switch: N letters
  procedure Parse_Switch (Name : in String; Str : in String;
                          Switch : out Switch_Definition) is
    Index : Switch_Index;
  begin
    -- Must be pairs of letter, max 26 pairs
    if Str'Length rem 2 /= 0 
    or else Str'Length > Natural(Switch_Range'Last) * 2 then
      Error ("Invalid number of letters in " &
             Name & " switch definition " & Str);
    end if;
    -- Init size of switch definition
    Switch := (Str'Length / 2, (others => (Types.Letter'First,
                                           Types.Letter'First)) );

    -- Store letters
    for I in Str'Range loop
      -- Check they are letters
      if Str(I) not in Types.Letter then
        Error ("Invalid letter " & Str(I) & " in " &
             Name & " switch definition " & Str);
      end if;
      if I rem 2 = 1 then
        Index := Switch_Index (I / 2 + 1);
        -- Encoding part: check unicity and store
        for J in 1 .. Index - 1 loop
          if Switch.Switch(J).E = Str(I) then
            Error ("Dual first letter " & Str(I) & " in " &
                   Name & " switch definition " & Str);
          end if;
        end loop;
        Switch.Switch(Index).E := Str(I);
      else
        -- Decoding part: check unicity and store
        for J in 1 .. Index - 1 loop
          if Switch.Switch(J).D = Str(I) then
            Error ("Dual second letter " & Str(I) & " in " &
                   Name & " switch definition " & Str);
          end if;
        end loop;
        Switch.Switch(Index).D := Str(I);
      end if;
    end loop;
  end Parse_Switch;

  -- Parse a scrambler: a number and a letter
  procedure Parse_Scrambler (Name : in String; Str : in String;
                             Scrambler : out Scrambler_Definition) is
  begin
    -- Two letters
    if Str'Length /= 2 then
      Error ("Invalid scrambler definition " & Str & " for " & Name);
    end if;
    -- Scrambler number
    begin
      Scrambler.Scrambler := Scrambler_Index'Value(
                              Str(Str'First .. Str'First));
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler number " & Str(Str'First) & " for " & Name);
    end;
    -- Scrambler offset
    begin
      Scrambler.Offset := Str(Str'Last);
    exception
      when Constraint_Error =>
        Error ("Invalid scrambler offset " & Str(Str'Last) & " for " & Name);
    end;
  end Parse_Scrambler;

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
        Error ("Invalid start index: " & Get_Parameter (1, Start_Key));
    end;
    -- Parse first switch
    begin
      Parse_Switch ("first", Get_Parameter (1, First_Switch_Key),
                    Parsed_Rec.First_Switch);
    exception
      when Argument_Not_Found =>
        null;
    end;
    -- Parse back
    begin
      Parse_Scrambler ("back", Get_Parameter (1, Back_Key), Parsed_Rec.Back);
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
        -- Check positive and odd number of chars (pairs of Num, Offset)
        if Str'Length < 2 or else Str'Length rem 2 /= 0 then
          Error ("Invalid jammers definition " & Str);
        end if;
        -- Check max number of jammers
        if Str'Length / 2 > Natural(Jammers_Range'Last) then
          Error ("To many jammers defined in " & Str);
        end if;
        Parsed_Rec.Jammers := (Str'Length / 2,
                               (others => (Scrambler_Index'First,
                                          Types.Letter'First)) );
        for I in 1 .. Str'Length / 2 loop
          Index := Jammers_Index(I);
          Parse_Scrambler ("jammer" & I'Img,
                           Str(I*2-1 .. I*2),
                           Parsed_Rec.Jammers.Jammers(Index) );
          -- Check unicity
        end loop;
      end;
    exception
      when Argument_Not_Found =>
        null;
    end;
    -- Parse last switch
    begin
      Parse_Switch ("last", Get_Parameter (1, Last_Switch_Key),
                    Parsed_Rec.Last_Switch);
    exception
      when Argument_Not_Found =>
        null;
    end;
  end Parse;

end Definition;

