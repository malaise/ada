-- Enigma expects <reflector_def> -r<rotor_def> -i<rotor_init> -s<switches>
-- <reflector_def> is a <reflector_name>@<letter_offset>
-- <rotor_def> is a list of <rotor_name>@<letter_ring_offset>#...
-- <rotor_init> is a list of offset letters (one for each rotor)
-- <switches> is pairs of letters, each letter appears at most once
-- See Def_Enigma.txt for more information

with Ada.Calendar;
with As.B, Perpet, Argument, Day_Mng, Normal, Upper_Str, Rnd,
     Num_Letters, Basic_Proc, Str_Util, Parser, Trace.Loggers;
with Types, Scrambler_Gen, Definition;
procedure Def_Enigma is

  Logger : Trace.Loggers.Logger;

  package Xml is
    -- Parse the Xml config file
    procedure Init;
    -- Get the Name of a rotor of reflector, given its id. "" if not found
    function Get_Name (Rotor : Boolean; Id : Positive) return String;
    -- Get the Id of a rotor of reflector, given its name. 0 if not found
    function Get_Id (Rotor : Boolean; Name : String) return Natural;
    Invalid_Configuration : exception;
  end Xml;
  package body Xml is separate;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Syntax error.");
    Basic_Proc.Put_Line_Error (" Usage: "
      & Argument.Get_Program_Name & " [ -text ] [ today | <date> | rnd | <text_key> | <enigma_setting> ]");
    Basic_Proc.Put_Line_Error ("  <date> ::= dd/mm/yyyy");
    Basic_Proc.Put_Line_Error ("  <enigma_setting> ::= <back> [ <rotors> <init> ] [ <switches> ]");
    Basic_Proc.Set_Error_Exit_Code;
  end Usage;

  function Is_Digit (C : Character) return Boolean is
  begin
    return C >= '0' and then C <= '9';
  end Is_Digit;

  function Is_Digit (S : String) return Boolean is
  begin
    for I in S'Range loop
      if not Is_Digit (S(I)) then
        return False;
      end if;
    end loop;
    return True;
  end Is_Digit;

  -- Argument parsing, action
  type Action_List is (Current_Date, Parse_Date, Random, Extract, Key);
  Action : Action_List := Current_Date;
  To_Text : Boolean := False;
  Nb_Arg : Natural;
  Other_Arg : Natural;

  -- For random generation
  -- Letter index 1 .. 26
  subtype Id_Range is Positive range 1 .. Types.Nb_Letters;
  function Id_Random is new Rnd.Discr_Random (Id_Range);

  -- String image
  Letter_Offset : constant := Character'Pos('A') - 1;
  function To_Letter (Id : Id_Range) return Character is
  begin
    return Character'Val(Id + Letter_Offset);
  end To_Letter;
  function To_Id (L : Character) return Id_Range is
  begin
    return Character'Pos(L) - Letter_Offset;
  end To_Id;

  -- For date generation
  -- Date
  Day   : Ada.Calendar.Day_Number;
  Month : Ada.Calendar.Month_Number;
  Year  : Ada.Calendar.Year_Number;
  T : Ada.Calendar.Time;
  Txt : As.B.Asb_Bs(256);

  Day_Month : As.B.Asb_Bs(18); -- WEDNESDAYSEPTEMBER
  -- The reflector used when generating from date
  Reflector_Num : Positive;

  -- For all
  Switch : As.B.Asb_Bs(26);
  Reflector : As.B.Asb_Bs(2);
  Rotors : As.B.Asb_Bs(8); -- 1 letter for rotor Id and 1 for ring
  Init_Offset : As.B.Asb_Bs(4);
  Nb_Rotors : Natural;
  Separator : Character;

  -- For unicity of rotors
  subtype Rotor_Id is Positive range 1 .. 10;
  Rotor_Nums : array (Definition.Rotors_Id_Range) of Rotor_Id;

  -- For text key parsing and extraction
  Valid : Boolean;
  subtype Prev_Scrambler_Range is Natural range 0 .. Rotor_Id'Last;
  Start, Stop : Natural;
  Prev_Scrambler : Prev_Scrambler_Range;
  Got_Scrambler : Rotor_Id;
  Got_Letters : array (1 .. 2) of Types.Letter;

  -- For Key parsing
  Reflector_Txt, Rotors_Txt, Init_Txt, Switches_Txt : As.B.Asb_Bs(256);
  Key_Error : exception;

  -- Parse a reflector definition
  procedure Parse_Reflector (Reflector_Str : String) is
    Arob : Natural;
    Id : Natural;
  begin
    Arob := Reflector_Str'Last - 1;
    if Reflector_Str (Arob) /= '@' then
      raise Key_Error;
    end if;
    Id := Xml.Get_Id (False, Reflector_Str(Reflector_Str'First .. Arob - 1));
    if Id = 0 then
      raise Key_Error;
    end if;
    Reflector.Set (To_Letter (Id) & Reflector_Str(Arob + 1));
  exception
    when others =>
      raise Key_Error;
  end Parse_Reflector;

  -- Input: N in 1 .. 10, to insert at Index
  function Store (N : Rotor_Id; Index : in Definition.Rotors_Id_Range)
                 return Rotor_Id is
    Id : Rotor_Id;
    Done : Boolean;
  begin
    Id := N;
    -- Check not used in 1 .. Index - 1
    loop
      Done := True;
      for I in 1 .. Index - 1 loop
        if Rotor_Nums(I) = Id then
          -- Id exists. Increment and re-check.
          if Id /= Rotor_Id'Last then
            Id := Id + 1;
          else
            Id := Rotor_Id'First;
          end if;
          Done := False;
          exit;
        end if;
      end loop;
      exit when Done;
    end loop;
    Rotor_Nums (Index) := Id;
    return Id;
  end Store;

  -- Stop is 0 if not found
  -- Look for ONE .. TEN in Str (Start .. Last)
  procedure Get_Number (Str : in String; Start : in Positive;
                        Last : out Natural; Id : out Rotor_Id) is
  begin
    Id := 1;
    Last := 0;
    -- Search if Str (Start .. Start+4) is a number in letters
    for Stop in Start + 2 .. Start + 4 loop
      if Stop > Str'Last then
        -- Nothing found and end of string
        return;
      end if;
      for I in Rotor_Id loop
        if Upper_Str (Num_Letters.Letters_Of(I)) = Str(Start .. Stop) then
          -- Found a num (ONE .. NINE, TEN), return 1 .. 10
          Id := I;
          Last := Stop;
          return;
        end if;
      end loop;
    end loop;
    -- Not found
    return;
  end Get_Number;


begin

  Nb_Arg := Argument.Get_Nbre_Arg;
  Other_Arg := 1;
  -- Check -text
  begin
    if Argument.Get_Parameter (1, "text") = "" then
      To_Text := True;
      Nb_Arg := Nb_Arg - 1;
      if Argument.Get_Position (1, "text") /= 1 then
        Usage;
        return;
      end if;
      Other_Arg := 2;
    else
      Usage;
      return;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      -- No "-text"
      null;
  end;


  if Nb_Arg = 0 then
    Usage;
    return;
  elsif Nb_Arg = 1 then
    if Argument.Get_Parameter (Occurence => Other_Arg) = "today" then
      Action := Current_Date;
    elsif Argument.Get_Parameter (Occurence => Other_Arg) = "rnd" then
      Action := Random;
    elsif Str_Util.Locate (Argument.Get_Parameter (Occurence => Other_Arg),
                             "/") /= 0 then
      -- Looks like a date
      Action := Parse_Date;
    else
      -- Try to extract a valid key from text, or a valid reflector def
      Action := Extract;
    end if;
  else
    -- Reflector + options (rotors, switches)
    Action := Key;
  end if;

  -- Check Key arguments
  if Action = Key then
    -- First level of checks
    -- 1 back, at most one rotor and init, at most one settings
    declare
      Rotor_Key : constant String := "r";
      Init_Key : constant String := "i";
      Switches_Key : constant String := "s";
      use Argument;
    begin
      if Is_Set (2, Not_Key) then
        raise Key_Error;
      end if;
      Reflector_Txt.Set (Get_Parameter (1, Not_Key));
      Nb_Arg := Other_Arg;
      if Is_Set (1, Rotor_Key) then
        if Is_Set (2, Rotor_Key) then
          raise Key_Error;
        end if;
        Rotors_Txt.Set (Get_Parameter (1, Rotor_Key));
        Nb_Arg := Nb_Arg + 1;
      end if;
      if Is_Set (1, Init_Key) then
        if Is_Set (2, Init_Key) then
          raise Key_Error;
        end if;
        Init_Txt.Set (Get_Parameter (1, Init_Key));
        Nb_Arg := Nb_Arg + 1;
      end if;
      if Is_Set (1, Switches_Key) then
        if Is_Set (2, Switches_Key) then
          raise Key_Error;
        end if;
        Switches_Txt.Set (Get_Parameter (1, Switches_Key));
        Nb_Arg := Nb_Arg + 1;
      end if;
      if Get_Nbre_Arg /= Nb_Arg then
        raise Key_Error;
      end if;
    exception
      when others =>
        raise Key_Error;
    end;
  elsif Nb_Arg /= 1 then
    -- One remaining arg
    Usage;
    return;
  end if;

  Xml.Init;

  -- Main processing of actions
  case Action is
    when Current_Date =>
      -- Current date: set Year Month Day for further generation
      T := Ada.Calendar.Clock;
      declare
        Dummy_Duration : Ada.Calendar.Day_Duration;
      begin
        Ada.Calendar.Split (T, Year, Month, Day, Dummy_Duration);
      end;
      Txt.Set (Normal (Day,   2, Gap => '0') & "/"
             & Normal (Month, 2, Gap => '0') & "/"
             & Normal (Year,  4, Gap => '0') );

    when Parse_Date =>
      -- Parse date: set Year Month Day for further generation
      begin
        Txt.Set (Argument.Get_Parameter (Occurence => Other_Arg));
      exception
        when Argument.Argument_Too_Long =>
          Usage;
          return;
      end;
      if Txt.Length = 10
      and then Txt.Element (3) = '/'
      and then Txt.Element (6) = '/' then
        if not Is_Digit (Txt.Slice (1, 2))
        or else not Is_Digit (Txt.Slice (4, 5))
        or else not Is_Digit (Txt.Slice (7, 10)) then
          Usage;
          return;
        end if;

        begin
          Day   := Ada.Calendar.Day_Number'Value   (Txt.Slice (1, 2));
          Month := Ada.Calendar.Month_Number'Value (Txt.Slice (4, 5));
          Year  := Ada.Calendar.Year_Number'Value  (Txt.Slice (7, 10));
        exception
          when others =>
            Usage;
            return;
        end;
      else
        Usage;
        return;
      end if;

    when Random =>
      Rnd.Gen.Randomize;
      -- Set random switches, not empty
      loop
        declare
          -- Generate a random symetric scrambler
          Str : constant String := Scrambler_Gen.Generate (True);
          Used : String (Str'Range) := (others => ' ');
          -- Generate a random number of switch entries
          N : constant Natural := Rnd.Gen.Int_Random (1, Id_Range'Last / 2);
        begin
          for I in 1 .. N loop
            -- Skip identity and dual definition
            if To_Letter(I) /= Str(I)
            and then Used(I) = ' ' then
              Switch.Append (To_Letter (I) & Str(I));
              Used(I) := Str(I);
              Used(To_Id (Str(I))) := To_Letter (I);
            end if;
          end loop;
        end;
        exit when not Switch.Is_Null;
      end loop;

      -- Set random number (3 to 4) of random rotors and rotor settings
      -- rotor 10 has N°0
      Nb_Rotors := Rnd.Gen.Int_Random (3, 4);
      declare
        Rot_Num : Rotor_Id;
      begin
        for I in 1 .. Nb_Rotors loop
          if Nb_Rotors /= 4 or else I /= 1 then
            -- Not the the first of 4 rotors: any of the 10 first rotors
            Rot_Num := Rnd.Gen.Int_Random (Rotor_Id'First, Rotor_Id'Last);
          else
            -- First rotor of 4: Any of 11th or 12th rotor
            Rot_Num := Rnd.Gen.Int_Random (1, 2);
          end if;
          Rot_Num := Store (Rot_Num, I);
          Rotors.Append (To_Letter (Rot_Num)
                       & To_Letter (Id_Random (Rnd.Gen.all)));
          Init_Offset.Append (To_Letter (Id_Random (Rnd.Gen.all)));
        end loop;
      end;

      -- Set random reflector, no offset
      if Nb_Rotors /= 4 then
        -- 3 rotors: Reflector A to C
        Reflector.Set (To_Letter (Rnd.Gen.Int_Random (1, 3)) & 'A');
      else
        -- 4 rotors: Reflectors Bthin or Cthin
        Reflector.Set (To_Letter (Rnd.Gen.Int_Random (4, 5)) & 'A');
      end if;

    when Extract =>
      begin
        Txt.Set (Argument.Get_Parameter (Occurence => Other_Arg));
      exception
        when Argument.Argument_Too_Long =>
          Usage;
          return;
      end;
      -- At least SSxxxTTZ (separator ONE|TWO|SIX|TEN terminator and Z)
      -- separator (and terminator) is twice the same letter
      Valid := Txt.Length >= 8;
      if Valid then
        -- Locate separator between switch and scramblers
        -- This is a letter repeated twice at odd offset
        Start := 0;
        declare
          Str : constant String := Txt.Image;
        begin
          for I in 1 .. Str'Last - 1 loop
            if Str(I) = Str(I + 1) then
              if I rem 2 = 1 then
                -- Separator found
                Start := I;
              elsif I + 2 <= Str'Last and then Str(I) = Str(I + 2) then
                -- Separator is the same as last letter of switch
                Start := I + 1;
              else
                -- Invalid duplication
                Valid := False;
              end if;
              exit;
            end if;
          end loop;
        end;

        if Start > Txt.Length - 7 then
          Valid := False;
        end if;
      end if;
      if Valid then
        Switch.Set (Txt.Slice (1, Start - 1));
        -- Skip the separator
        Start := Start + 2;
        -- Look for scramblers
        Prev_Scrambler := 0;
        Got_Letters := (others => 'A');
        loop
          if Txt.Length >= Start
          and then Txt.Element (Start) = 'Z' then
            if Prev_Scrambler = 0 then
              -- Empty definition!
              Valid := False;
              exit;
            end if;
            -- End of definition of rotors and reflector
            if Got_Letters(1) /= Got_Letters(2) then
              -- For the reflector: same letter
              Valid := False;
              exit;
            end if;
            Reflector.Set (To_Letter (Prev_Scrambler) & Got_Letters(1));
            -- Will exit with code = first significant index after key
            Start := Start + 1;
            exit;
          end if;

          -- Look for scrambler num in letter
          Get_Number (Txt.Image, Start, Stop, Got_Scrambler);
          if Stop = 0 then
            Valid := False;
            exit;
          end if;
          if Prev_Scrambler /= 0 then
            -- Prev rotor parsed ok: store rotor num and ring offset,
            -- and rotor initial offset
            Rotors.Append (To_Letter (Prev_Scrambler) & Got_Letters(1));
            Init_Offset.Append (Got_Letters(2));
          end if;
          -- Two letters (ring offset and initial offset,
          --  or twice the reflector offset)
          begin
            Got_Letters(1) := Txt.Element (Stop + 1);
          exception
            when Constraint_Error =>
              -- No more char or no a letter
              Valid := False;
              exit;
          end;
          begin
            Got_Letters(2) := Txt.Element (Stop + 2);
          exception
            when Constraint_Error =>
              -- No more char or no a letter
              Valid := False;
              exit;
          end;
          Prev_Scrambler := Got_Scrambler;
          Start := Stop + 3;
        end loop;
      end if;

      if Valid then
        -- Exit with next significant index
        Basic_Proc.Set_Exit_Code (Start);
      else
        -- Parse as a reflector definition
        Parse_Reflector (Txt.Image);
      end if;
  when Key =>
    -- Get rotors init, get Nb of rotors
    begin
      Init_Offset.Set (Init_Txt);
      Nb_Rotors := Init_Offset.Length;
      if Nb_Rotors > 4 then
        raise Key_Error;
      end if;
    exception
      when others =>
        raise Key_Error;
    end;

    -- Extract reflector
    Parse_Reflector (Reflector_Txt.Image);

    -- Extract rotors and check Nb versus offsets
    declare
      -- Delimiter in Rotors string
      function Separing (C : Character) return Boolean is
      begin
        return C = '#';
      end Separing;
      Iter : Parser.Iterator;
      Id : Natural;
    begin
      -- Parse the Rotors definition
      -- <Name>@<OffsetLetter> [ { #<Name>@<OffsetLetter> } ]
      Iter.Set (Rotors_Txt.Image , Separing'Unrestricted_Access);
      for I in 1 .. Nb_Rotors loop
        declare
          Str : constant String := Iter.Next_Word;
          Arob : Natural;
        begin
          if Str = "" then
            raise Key_Error;
          end if;
          Arob := Str_Util.Locate (Str, "@");
          if Arob /= Str'Last - 1 then
            raise Key_Error;
          end if;
          -- Check ring and init offset letter
          if Str(Str'Last) not in Types.Letter then
            raise Key_Error;
          end if;
          if Init_Offset.Element (I) not in Types.Letter then
            raise Key_Error;
          end if;
          -- Check and adjust rotor num
          Id := Xml.Get_Id (True, Str(Str'First .. Arob - 1));
          if Id = 0 then
            raise Key_Error;
          end if;
          Rotors.Append (To_Letter (Id) & Str(Str'Last));
        end;
      end loop;

      -- No more Rotor allowed
      if Iter.Next_Word /= "" then
        raise Key_Error;
      end if;
      Iter.Del;
    exception
      when others =>
        raise Key_Error;
    end;

    -- Extract and check switches
    declare
      Str : constant String := Switches_Txt.Image;
      Init_Str : String (1 .. Types.Nb_Letters);
    begin
      Switch.Set (Switches_Txt);
      if Switch.Length mod 2 /= 0 then
        raise Key_Error;
      end if;
      Init_Str := (others => ' ');
      for I in 1 .. Str'Length loop
        if I mod 2 = 1 then
          if Str(I) = Str(I + 1) then
            raise Key_Error;
          end if;
          if      Init_Str (To_Id (Str(I))) /= ' '
          or else Init_Str (To_Id (Str(I + 1))) /= ' ' then
            raise Key_Error;
          end if;
          Init_Str(To_Id (Str(I))) := Str(I+1);
          Init_Str(To_Id (Str(I+1))) := Str(I);
        end if;
      end loop;
    exception
      when others =>
        raise Key_Error;
    end;

  end case;

  -- Build keys from date
  if Action = Current_Date or else Action = Parse_Date then
    -- Build time of 0h00 of date
    declare
      Hour     : constant Day_Mng.T_Hours    := 0;
      Minute   : constant Day_Mng.T_Minutes  := 0;
      Second   : constant Day_Mng.T_Seconds  := 0;
      Millisec : constant Day_Mng.T_Millisec := 0;
    begin
      T := Ada.Calendar.Time_Of (Year, Month, Day,
                 Day_Mng.Pack (Hour, Minute, Second, Millisec));
    exception
      when others =>
        Usage;
        return;
    end;
    -- Switch definition from Day, Month and Year
    -- Concatenate day and month names in uppercase
    Day_Month.Set (
      Upper_Str (Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T))));
    Day_Month.Append (
      Upper_Str (Perpet.Month_Name_List'Image (Perpet.Get_Month_Name (Month))));

    -- Unicity of each letter
    declare
      Str : String := Day_Month.Image;
      I, J, L : Natural;
    begin
      I := 1;
      L := Str'Last;
      loop
        J := I + 1;
        exit when J > L;
        loop
          if Str(J) = Str(I) then
            -- Str(J) already exists: shift left
            Str (J .. L - 1) := Str (J + 1 .. L);
            L := L - 1;
          else
            J := J + 1;
          end if;
          exit when J > L;
        end loop;
        I := I + 1;
        exit when I >= L;
      end loop;
      -- 13 pairs max => 26 letters max
      if L > Id_Range'Last then
        L := Id_Range'Last;
      end if;
      -- Length must be even
      if L rem 2 /= 0  then
        L := L - 1;
      end if;
      Switch.Set (Str(1 .. L));
    end;

    -- Reflector: daynum 1 -> A, 2 -> B, 3 -> C, 1 -> A...
    Reflector_Num := ((Day - 1) rem 3) + 1;
    Reflector.Set (To_Letter (Reflector_Num) & 'A');

    declare
      Month_3 : String (1 .. 3);
      Day_3 : String (1 .. 3);
      Num : Rotor_Id;
    begin
      -- Rotors: ring setting and offset
      Month_3 := Upper_Str (Perpet.Month_Name_List'Image
                         (Perpet.Get_Month_Name (Month))) (1 .. 3);
      Day_3 := Upper_Str (
          Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T)))(1..3);

      -- First rotor: day modulo 1 .. 10
      Num := Store (((Day - 1) rem 10) + 1, 1);
      Rotors.Set (To_Letter (Num) & Day_3(1));
      Init_Offset.Set (Month_3(1));
      -- Second rotor Month / 10
      Num := Store ((Day / 10) + 1, 2);
      Rotors.Append (To_Letter (Num) & Day_3(2));
      Init_Offset.Append (Month_3(2));
      -- Third rotor
      Num := Store (((Month - 1) rem 10) + 1, 3);
      Rotors.Append (To_Letter (Num) & Day_3(3));
      Init_Offset.Append (Month_3(3));
    end;
  end if;

  -- Put setting in internal format
  Logger.Log_Debug (Reflector.Image
                  & " r=" & Rotors.Image
                  & " i=" & Init_Offset.Image
                  & " s=" & Switch.Image);

  -- Result
  -- Put normal enigma keys
  declare
    Num : constant Positive := To_Id (Reflector.Element (1));
  begin
    Basic_Proc.Put_Output (Xml.Get_Name (False, Num) & '@'
                   & Reflector.Element (2));
  end;

  if not Init_Offset.Is_Null then
    Basic_Proc.Put_Output (" -r");
    for I in 1 .. Rotors.Length loop
      if I mod 2 = 1 then
        if I /= 1 then
          Basic_Proc.Put_Output ('#');
        end if;
        declare
          Name : constant String
               := Xml.Get_Name (True, To_Id (Rotors.Element (I)));
        begin
          Basic_Proc.Put_Output (Name & '@');
        end;
      else
        Basic_Proc.Put_Output (Rotors.Element (I));
      end if;
    end loop;
    Basic_Proc.Put_Output (" -i" & Init_Offset.Image);
  end if;

  if not Switch.Is_Null then
    Basic_Proc.Put_Output (" -s" & Switch.Image);
  end if;

  -- Result
  -- Text output
  if To_Text then
    Basic_Proc.Put_Output (" ");
    -- Key coded onto text
    -- Switch and random separator
    Separator := To_Letter (Id_Random (Rnd.Gen.all));
    Basic_Proc.Put_Output (Switch.Image & Separator & Separator);
    for I in 1 .. Rotors.Length loop
      if I rem 2 = 1 then
        -- Rotor letter
        declare
          Num : constant Positive
              := Positive (To_Id (Rotors.Element (I)) );
        begin
          Basic_Proc.Put_Output (Upper_Str (Num_Letters.Letters_Of (Num)));
        end;
        -- Ring offset
        Basic_Proc.Put_Output (Rotors.Element (I + 1));
        -- Initial offset
        Basic_Proc.Put_Output (Init_Offset.Element ((I - 1) / 2 + 1));
      end if;
    end loop;
    -- Reflector: Num, offset, offset and zero
    declare
      Reflector_Num : constant Positive
                    := Positive (To_Id (Reflector.Element (1)));

      Reflector_Str : constant String
                := Upper_Str (Num_Letters.Letters_Of (Reflector_Num));
    begin
      Basic_Proc.Put_Output (Reflector_Str);
      Basic_Proc.Put_Output (Reflector.Element (2));
      Basic_Proc.Put_Output (Reflector.Element (2));
      Basic_Proc.Put_Output ('Z');
    end;
  end if;

  Basic_Proc.New_Line_Output;
exception
  when Key_Error =>
    Basic_Proc.Put_Line_Error ("Invalid key specification.");
    Usage;
end Def_Enigma;

