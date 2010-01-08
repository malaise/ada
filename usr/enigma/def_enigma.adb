with Ada.Calendar, Ada.Text_Io;
with Perpet, Argument, Day_Mng, Normal, Text_Handler, Upper_Str, Rnd,
     Num_Letters, Sys_Calls, String_Mng;
with Types, Scrambler_Gen;
procedure Def_Enigma is
  -- Constants
  Back_Scrambler : constant := 8;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Syntax error.");
    Sys_Calls.Put_Line_Error (" Usage: "
      & Argument.Get_Program_Name & " [ -text ] [ <date> | rnd | <text_key> ]");
    Sys_Calls.Put_Line_Error ("    <date> ::= dd/mm/yyyy");
    Sys_Calls.Set_Error_Exit_Code;
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

  type Action_List is (Current_Date, Parse_Date, Random, Extract);
  Action : Action_List := Current_Date;
  To_Text : Boolean := False;
  Nb_Arg : Natural;
  Other_Arg : Natural;

  -- For random generation
  -- Letter index 1 .. 26
  subtype Id_Range is Positive range 1 .. Positive(Types.Lid'Last) + 1;
  function Id_Random is new Rnd.Discr_Random (Id_Range);

  -- String image
  Offset : constant := Character'Pos('A') - 1;
  function To_Letter (Id : Id_Range) return Character is
  begin
    return Character'Val(Id + Offset);
  end To_Letter;

  -- For date generation
  -- Date
  Day   : Ada.Calendar.Day_Number;
  Month : Ada.Calendar.Month_Number;
  Year  : Ada.Calendar.Year_Number;
  T : Ada.Calendar.Time;
  Txt : Text_Handler.Text (256);

  Day_Month : Text_Handler.Text (18); -- WEDNESDAYSEPTEMBER
  Day_26 : Natural; -- Day rem 26
  Month_3 : String (1 .. 3);
  Day_3 : String (1 .. 3);

  -- For all
  Switch : Text_Handler.Text (26 * 2);
  Back : Text_Handler.Text (2);
  Jammers : Text_Handler.Text (16);
  Rotors : Text_Handler.Text (8);

  -- For unicity of scramblers
  type Digit is mod 9; -- 0 .. 8 (that will lead to 1 .. 9)
  Nums : array (1 .. 9) of Digit;
  subtype Nat_9 is Natural  range 0 .. 9;
  subtype Pos_9 is Positive range 1 .. 9;
  Num : Pos_9;
  -- I gets its unit (last digit) extracted then wrapped
  -- 0&9->1, 1->2
  function Wrap (I : Natural) return Pos_9 is
  begin
    if I rem 10 = 9 then
      return 1;
    else
      return I rem 10 + 1;
    end if;
  end Wrap;

  -- Input: N in 1 .. 9, to insert at Index
  function Store (N : in Pos_9; Index : in Positive) return Pos_9 is
    V : Digit;
    Done : Boolean;
  begin
    -- Get a val 0 .. 8
    V := Digit((N-1) rem Digit'Modulus);
    -- Check not used in 1 .. Index - 1
    loop
      Done := True;
      for I in 1 .. Index - 1 loop
        if Nums(I) = V then
          -- V exists. Increment and re-check.
          V := V + 1;
          Done := False;
          exit;
        end if;
      end loop;
      exit when Done;
    end loop;
    Nums (Index) := V;
    return Nat_9(V) + 1;
  end Store;

  -- For extraction
  -- Num and Stop are 0 if not found
  Separator : constant String := "JJJ";
  procedure Get_Number (Str : in String; Start : in Positive;
                        Last : out Natural; Num : out Nat_9) is
  begin
    Num := 0;
    Last := 0;
    -- Search if Str (Start .. Start+4) is a number in letters
    for Stop in Start + 2 .. Start + 4 loop
      if Stop > Str'Last then
        -- Nothing found and end of string
        return;
      end if;
      for I in Pos_9'Range loop
        if Upper_Str (Num_Letters.Letters_Of(I)) = Str(Start .. Stop) then
          -- Found a num
          Num := I;
          Last := Stop;
          return;
        end if;
      end loop;
    end loop;
    -- Not found
    return;
  end Get_Number;
  Start, Stop : Natural;
  Prev_Scrambler, Got_Scrambler : Nat_9;
  Got_Letters : array (1 .. 2) of Types.Letter;

begin

  Nb_Arg := Argument.Get_Nbre_Arg;
  Other_Arg := 1;
  -- Check -text
  if Nb_Arg /= 0 then
    begin
      if Argument.Get_Parameter (1, "text") = "" then
        To_Text := True;
        Nb_Arg := Nb_Arg - 1;
        if Argument.Get_Position (1, "text") = 1 then
          Other_Arg := 2;
        end if;
      else
        Usage;
        return;
      end if;
    exception
      when Argument.Argument_Not_Found =>
        -- No "-text"
        null;
    end;
  end if;
  -- At most one remaining arg
  if Nb_Arg > 1 then
    Usage;
    return;
  end if;

  if Nb_Arg = 0 then
    Action := Current_Date;
  elsif Argument.Get_Parameter (Occurence => Other_Arg) = "rnd" then
    Action := Random;
  elsif String_Mng.Locate (Argument.Get_Parameter (Occurence => Other_Arg),
                           "/") /= 0 then
    -- Looks like a date
    Action := Parse_Date;
  elsif String_Mng.Locate (Argument.Get_Parameter (Occurence => Other_Arg),
                           Separator) /= 0 then
    -- Looks like a text key
    Action := Extract;
  else
    Usage;
    return;
  end if;

  case Action is
    when Current_Date =>
      -- Current date: set Year Month Day for further generation
      T := Ada.Calendar.Clock;
      declare
        Dummy_Duration : Ada.Calendar.Day_Duration;
      begin
        Ada.Calendar.Split (T, Year, Month, Day, Dummy_Duration);
      end;
      Text_Handler.Set (Txt, Normal (Day,   2, Gap => '0') & "/"
                           & Normal (Month, 2, Gap => '0') & "/"
                           & Normal (Year,  4, Gap => '0') );

    when Parse_Date =>
      -- Parse date: set Year Month Day for further generation
      begin
        Argument.Get_Parameter (Txt, Occurence => Other_Arg);
      exception
        when Argument.Argument_Too_Long =>
          Usage;
          return;
      end;
      if Text_Handler.Length (Txt) = 10
      and then Text_Handler.Value (Txt)(3) = '/'
      and then Text_Handler.Value (Txt)(6) = '/' then
        if not Is_Digit (Text_Handler.Value (Txt)(1 .. 2))
        or else not Is_Digit (Text_Handler.Value (Txt)(4 .. 5))
        or else not Is_Digit (Text_Handler.Value (Txt)(7 .. 10)) then
          Usage;
          return;
        end if;

        begin
          Day   := Ada.Calendar.Day_Number'Value   (Text_Handler.Value (Txt)(1 .. 2));
          Month := Ada.Calendar.Month_Number'Value (Text_Handler.Value (Txt)(4 .. 5));
          Year  := Ada.Calendar.Year_Number'Value  (Text_Handler.Value (Txt)(7 .. 10));
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
      Rnd.Randomize;
      -- Set random switches
      declare
        -- Generate a random full asymetric scrambler
        Str : constant String := Scrambler_Gen.Generate (False);
        -- Generate a random number of switch entries
        N : constant Natural := Rnd.Int_Random (0, Id_Range'Last);
        -- Init array of already mapped letters
        Used : array (Id_Range) of Boolean := (others => False);
        Index : Id_Range;
      begin
        for I in 1 .. N loop
          loop
            -- Look for a random unused char
            Index := Id_Random;
            exit when not Used(Index);
          end loop;
          Used(Index) := True;
          Text_Handler.Append (Switch, To_Letter(Index) & Str(Index));
        end loop;
      end;

      -- Set random back between 7 and 9
      declare
        Back_Num : Pos_9 := Id_Random (7, 9);
      begin
        Back_Num := Store (Back_Num, 1);
        Text_Handler.Set (Back, Normal(Back_Num, 1)
                              & To_Letter (Id_Random));
      end;

      -- Set random number (3 to 8) of random jammers and rotor settings
      declare
        Jam_Nb : constant Natural := Rnd.Int_Random (3, 8);
        Jam_Num : Pos_9;
      begin
        for I in 1 .. Jam_Nb loop
          Jam_Num := Id_Random (1, 9);
          Jam_Num := Store (Jam_Num, I + 1);
          Text_Handler.Append (Jammers, Normal(Jam_Num, 1)
                                      & To_Letter (Id_Random));
          Text_Handler.Append (Rotors, To_Letter (Id_Random));
        end loop;
      end;

    when Extract =>
      begin
        Argument.Get_Parameter (Txt, Occurence => Other_Arg);
      exception
        when Argument.Argument_Too_Long =>
          Usage;
          return;
      end;
      -- Locate separator between switch and scramblers
      Start := Text_Handler.Locate (Txt, Separator);
      -- Pairs of letters before separator
      if Start rem 2 /= 1 then
        Usage;
        return;
      end if;
      Text_Handler.Set (Switch, Text_Handler.Value(Txt) (1 .. Start - 1));
      -- Skip the separator
      Start := Start + Separator'Length;
      -- Look for scramblers
      Prev_Scrambler := 0;
      Got_Letters := (others => 'A');
      loop
        -- Look for scrambler num in letter
        Get_Number (Text_Handler.Value(Txt), Start, Stop, Got_Scrambler);
        if Stop = 0 then
          Usage;
          return;
        end if;
        if Got_Scrambler = Prev_Scrambler then
          -- Scrambler num repeated : this is a back and done
          if Got_Letters(1) /= Got_Letters(2) then
            -- For the back: same letter
            Usage;
            return;
          end if;
          Text_Handler.Set (Back, Normal(Got_Scrambler, 1) & Got_Letters(1));
          Start := Stop + 1;
          exit;
        elsif Prev_Scrambler /= 0 then
          -- Prev jammer parsed ok: store jammer num and carry, and rotor setting
          Text_Handler.Append (Jammers,
                  Normal(Prev_Scrambler, 1) & Got_Letters(2));
          Text_Handler.Append (Rotors, Got_Letters(1));
        end if;
        -- Two letters (offset and carry, same for back)
        begin
          Got_Letters(1) := Text_Handler.Value(Txt)(Stop + 1);
        exception
          when Constraint_Error =>
            -- No more char or no a letter
            Usage;
            return;
        end;
        begin
          Got_Letters(2) := Text_Handler.Value(Txt)(Stop + 2);
        exception
          when Constraint_Error =>
            -- No more char or no a letter
            Usage;
            return;
        end;
        Prev_Scrambler := Got_Scrambler;
        Start := Stop + 3;
      end loop;
      Sys_Calls.Set_Exit_Code (Start);
  end case;

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
    Text_Handler.Set (Day_Month,
      Upper_Str (Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T))));
    Text_Handler.Append (Day_Month,
      Upper_Str (Perpet.Month_Name_List'Image (Perpet.Get_Month_Name (Month))));

    -- Unicity of each letter
    declare
      Str : String := Text_Handler.Value(Day_Month);
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
      -- Length must be even
      if L rem 2 /= 0  then
        L := L - 1;
      end if;
      Text_Handler.Set (Switch, Str(1 .. L));
    end;

    -- Back
    Day_26 := (Day - 1) rem 26;
    Text_Handler.Set (Back, Normal(Back_Scrambler, 1)
      & Character'Val(Character'Pos('A') + Day_26));
    Num := Store (Back_Scrambler, 1);

    -- Jammers
    Month_3 := Upper_Str (Perpet.Month_Name_List'Image
                       (Perpet.Get_Month_Name (Month))) (1 .. 3);
    Day_3 := Upper_Str (
        Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T)))(1..3);
    Num := Store (Wrap (Day), 2);
    Text_Handler.Set (Jammers, Normal(Num, 1) & Day_3(1));
    Text_Handler.Set (Rotors, Month_3(1));
    Num := Store (Wrap (Month / 10), 3);
    Text_Handler.Append (Jammers, Normal(Num, 1) & Day_3(2));
    Text_Handler.Append (Rotors, Month_3(2));
    Num := Store (Wrap (Month rem 10), 4);
    Text_Handler.Append (Jammers, Normal(Num, 1) & Day_3(3));
    Text_Handler.Append (Rotors, Month_3(3));
  end if;

  -- Result
  -- Normal enigma args
  if not Text_Handler.Empty (Switch) then
    Ada.Text_Io.Put (" -s" & Text_Handler.Value (Switch));
  end if;
  if not Text_Handler.Empty (Jammers) then
    Ada.Text_Io.Put (" -j" & Text_Handler.Value (Jammers));
  end if;
  Ada.Text_Io.Put (" -b" & Text_Handler.Value (Back));
  if not Text_Handler.Empty (Jammers) then
    Ada.Text_Io.Put (" -r" & Text_Handler.Value (Rotors));
  end if;
  Ada.Text_Io.New_Line;

  if To_Text then
    -- Key coded onto text
    -- Switch and separator
    Ada.Text_Io.Put (Text_Handler.Value (Switch) & Separator);
    for I in 1 .. Text_Handler.Length (Jammers) loop
      if I rem 2 = 1 then
        -- Jammer letter
        Num := Pos_9'Value (Text_Handler.Value (Jammers)(I .. I));
        Ada.Text_Io.Put (Upper_Str (Num_Letters.Letters_Of (Num)));
        -- Offset
        Ada.Text_Io.Put (Text_Handler.Value (Rotors)((I-1)/2+1));
      else
        -- Carry
        Ada.Text_Io.Put (Text_Handler.Value (Jammers)(I));
      end if;
    end loop;
    -- Back: Num, offset, offset and num
    Num := Pos_9'Value (Text_Handler.Value (Back)(1 .. 1));
    Ada.Text_Io.Put (Upper_Str (Num_Letters.Letters_Of (Num)));
    Ada.Text_Io.Put (Text_Handler.Value (Back)(2));
    Ada.Text_Io.Put (Text_Handler.Value (Back)(2));
    Ada.Text_Io.Put (Upper_Str (Num_Letters.Letters_Of (Num)));
  end if;

end Def_Enigma;

