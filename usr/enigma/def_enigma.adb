with Ada.Calendar, Ada.Text_Io;
with Perpet, Argument, Day_Mng, Normal, Text_Handler, Upper_Str, Rnd;
with Types, Scrambler_Gen;
procedure Def_Enigma is
  -- Constants
  Back_Scrambler : constant := 8;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("Syntax error or invalid date.");
    Ada.Text_Io.Put_Line(" Usage: "
                 & Argument.Get_Program_Name & " [ <date> | rnd ]");
    Ada.Text_Io.Put_Line("    <date> ::= dd/mm/yyyy");
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

  Random : Boolean := False;

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
  Txt : Text_Handler.Text (10);

  Day_Month : Text_Handler.Text (18); -- WEDNESDAYSEPTEMBER
  Day_26 : Natural; -- Day rem 26
  Month_3 : String (1 .. 3);

  -- For both
  Switch : Text_Handler.Text (26 * 2);
  Back : Text_Handler.Text (2);
  Jammers : Text_Handler.Text (16);

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

begin

  if Argument.Get_Nbre_Arg = 0 then
    -- Current date
    T := Ada.Calendar.Clock;
    declare
      Dummy_Duration : Ada.Calendar.Day_Duration;
    begin
      Ada.Calendar.Split (T, Year, Month, Day, Dummy_Duration);
    end;
    Text_Handler.Set (Txt, Normal (Day,   2, Gap => '0') & "/"
                         & Normal (Month, 2, Gap => '0') & "/"
                         & Normal (Year,  4, Gap => '0') );
    Random := False;
  elsif Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter = "rnd" then
    Random := True;
  elsif Argument.Get_Nbre_Arg = 1 then
    -- Get date from arg 1
    Argument.Get_Parameter (Txt);
    if Text_Handler.Length (Txt) /= 10
    or else Text_Handler.Value (Txt)(3) /= '/'
    or else Text_Handler.Value (Txt)(6) /= '/' then
      Usage;
      return;
    end if;

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
    Random := False;
  else
    Usage;
    return;
  end if;

  if Random then
    Rnd.Randomize;

    -- Set random switch
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

    -- Set random number (3 to 8) of random jammers
    declare
      Jam_Nb : constant Natural := Rnd.Int_Random (3, 8);
      Jam_Num : Pos_9;
    begin
      for I in 1 .. Jam_Nb loop
        Jam_Num := Id_Random (1, 9);
        Jam_Num := Store (Jam_Num, I + 1);
        Text_Handler.Append (Jammers, Normal(Jam_Num, 1)
                                    & To_Letter (Id_Random));
      end loop;
    end;
 
  else

    -- Build time of 0h00 of date
    declare
      Hour     : Day_Mng.T_Hours    := 0;
      Minute   : Day_Mng.T_Minutes  := 0;
      Second   : Day_Mng.T_Seconds  := 0;
      Millisec : Day_Mng.T_Millisec := 0;
    begin
      T := Ada.Calendar.Time_Of (Year, Month, Day,
                 Day_Mng.Pack (Hour, Minute, Second, Millisec));
    exception
      when others =>
        Usage;
        return;
    end;

    -- Switch definition
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
    Num := Store (Wrap (Day), 2);
    Text_Handler.Set (Jammers, Normal(Num, 1) & Month_3(1));
    Num := Store (Wrap (Month / 10), 3);
    Text_Handler.Append (Jammers, Normal(Num, 1) & Month_3(2));
    Num := Store (Wrap (Month rem 10), 4);
    Text_Handler.Append (Jammers, Normal(Num, 1) & Month_3(3));
  end if;

  -- Result
  if not Text_Handler.Empty (Switch) then
    Ada.Text_Io.Put (" -s" & Text_Handler.Value (Switch));
  end if;
  if not Text_Handler.Empty (Jammers) then
    Ada.Text_Io.Put (" -j" & Text_Handler.Value (Jammers));
  end if;
  Ada.Text_Io.Put_Line (" -b" & Text_Handler.Value (Back));

end Def_Enigma;

