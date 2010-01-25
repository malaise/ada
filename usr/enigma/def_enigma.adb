-- Enigma expects -s<switches> -r<rotor_def> <reflector_def> -i<rotor_init>
-- <switches> is pairs of letters, each letter appears at most once
-- <rotor_def> is a list of <rotor_name>@<letter_ring_offset>#...
-- <reflector_def> is a <reflector_name>@<letter_offset>
-- <rotor_init> is a list of offset letters (one for each rotor)
-- See Def_Enigma.txt for more information

with Ada.Calendar, Ada.Text_Io;
with Perpet, Argument, Day_Mng, Normal, Text_Handler, Upper_Str, Rnd,
     Num_Letters, Sys_Calls, String_Mng;
with Types, Scrambler_Gen, Definition;
procedure Def_Enigma is
  -- Constants
  -- The reflector used when generating from date
  Reflector_Num : constant := 2;

  package Xml is
    -- Parse the Xml config file
    procedure Init;
    -- Get the Name of a rotor of reflector, given its id. "" if not found
    function Get_Name (Rotor : Boolean; Id : Positive) return String;
    Invalid_Configuration : exception;
  end Xml;
  package body Xml is separate;

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
  Txt : Text_Handler.Text (256);

  Day_Month : Text_Handler.Text (18); -- WEDNESDAYSEPTEMBER

  -- For all
  Switch : Text_Handler.Text (26 * 2);
  Reflector : Text_Handler.Text (2);
  Rotors : Text_Handler.Text (31); -- "SEVEN@A" * 4 + '#' * 3
  Init_Offset : Text_Handler.Text (4);

  -- For unicity of scramblers
  subtype Rotor_Id is Positive range 1 .. 10;
  Rotor_Nums : array (Definition.Rotors_Id_Range) of Rotor_Id;

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

  -- For extraction
  -- Stop is 0 if not found
  Separator : constant String := "JJJ";
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


  Start, Stop : Natural;
  subtype Prev_Scrambler_Range is Natural range 0 .. Rotor_Id'Last;
  Prev_Scrambler : Prev_Scrambler_Range;
  Got_Scrambler : Rotor_Id;
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

  Xml.Init;

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
      -- Set random switches, not empty
      loop
        declare
          -- Generate a random symetric scrambler
          Str : constant String := Scrambler_Gen.Generate (True);
          Used : String (Str'Range) := (others => ' ');
          -- Generate a random number of switch entries
          N : constant Natural := Rnd.Int_Random (1, Id_Range'Last / 2);
        begin
          for I in 1 .. N loop
            -- Skip identity and dual definition
            if To_Letter(I) /= Str(I)
            and then Used(I) = ' ' then
              Text_Handler.Append (Switch, To_Letter (I) & Str(I));
              Used(I) := Str(I);
              Used(To_Id (Str(I))) := To_Letter (I);
            end if;
          end loop;
        end;
        exit when not Text_Handler.Empty (Switch);
      end loop;

      -- Set random reflector between 1 and 5
      Text_Handler.Set (Reflector, Normal(Rnd.Int_Random (1, 5), 1)
                              & To_Letter (Id_Random));

      -- Set random number (3 to 4) of random rotors and rotor settings
      -- rotor 10 has N°0
      declare
        Rot_Nb : constant Natural := Rnd.Int_Random (3, 4);
        Rot_Num : Rotor_Id;
      begin
        for I in 1 .. Rot_Nb loop
          Rot_Num := Rnd.Int_Random (Rotor_Id'First, Rotor_Id'Last);
          Rot_Num := Store (Rot_Num, I);
          Text_Handler.Append (Rotors, Normal(Rot_Num rem 10, 1)
                                      & To_Letter (Id_Random));
          Text_Handler.Append (Init_Offset, To_Letter (Id_Random));
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
      -- This cannot be "JJJJJ", but can be "xJJJJ"!
      Start := Text_Handler.Locate (Txt, Separator(1) & Separator);
      if Start /= 0 then
        -- Yes it is, the real separator is after the first "J"
        Start := Start + 1;
      else
        -- This is "xyJJJ"
        Start := Text_Handler.Locate (Txt, Separator);
      end if;

      -- Pairs of letters before separator (=> separator found)
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
          -- Scrambler num repeated : this is a reflector and done
          if Got_Letters(1) /= Got_Letters(2) then
            -- For the reflector: same letter
            Usage;
            return;
          end if;
          Text_Handler.Set (Reflector,
                    Normal(Got_Scrambler, 1) & Got_Letters(1));
          Start := Stop + 1;
          exit;
        elsif Prev_Scrambler /= 0 then
          -- Prev rotor parsed ok: store rotor num and ring offset,
          -- and rotor initial offset
          Text_Handler.Append (Rotors,
                  Normal(Prev_Scrambler rem 10, 1) & Got_Letters(1));
          Text_Handler.Append (Init_Offset, Got_Letters(2));
        end if;
        -- Two letters (ring offset and initial offset, 
        --  or twice the reflector offset)
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
      if L > Id_Range'Last / 2 then
        L := Id_Range'Last / 2;
      end if;
      Text_Handler.Set (Switch, Str(1 .. L));
    end;

    -- Reflector
    Text_Handler.Set (Reflector, Normal (Reflector_Num, 1)
      & Types.Letter_Of (Types.Id_Of (Day)));

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
      Text_Handler.Set (Rotors, Normal(Num, 1) & Day_3(1));
      Text_Handler.Set (Init_Offset, Month_3(1));
      -- Second rotor Month / 10
      Num := Store ((Day / 10) + 1, 2);
      Text_Handler.Append (Rotors, Normal(Num, 1) & Day_3(2));
      Text_Handler.Append (Init_Offset, Month_3(2));
      -- Third rotor
      Num := Store (((Month - 1) rem 10) + 1, 3);
      Text_Handler.Append (Rotors, Normal(Num, 1) & Day_3(3));
      Text_Handler.Append (Init_Offset, Month_3(3));
    end;
  end if;

  -- Result
  -- Normal enigma args
  if not Text_Handler.Empty (Switch) then
    Ada.Text_Io.Put (" -s" & Text_Handler.Value (Switch));
  end if;
  if not Text_Handler.Empty (Rotors) then
    Ada.Text_Io.Put (" -r");
    for I in 1 .. Text_Handler.Length (Rotors) loop
      if I mod 2 = 1 then
        if I /= 1 then
          Ada.Text_Io.Put ('#');
        end if;
        declare
          Num : constant Natural
              := Natural'Value (Text_Handler.Value (Rotors)(I) & "");
        begin
          if Num /= 0 then
            Ada.Text_Io.Put (Xml.Get_Name (True, Num) & '@');
          else
            Ada.Text_Io.Put (Xml.Get_Name (True, 10) & '@');
          end if;
        end;
      else
        Ada.Text_Io.Put (Text_Handler.Value (Rotors)(I));
      end if;
    end loop;
  end if;
  declare
    Num : constant Positive
        := Positive'Value (Text_Handler.Value (Reflector)(1) & "");
  begin
    Ada.Text_Io.Put (" " & Xml.Get_Name (False, Num) & '@'
                   & Text_Handler.Value (Reflector)(2));
  end;
  if not Text_Handler.Empty (Rotors) then
    Ada.Text_Io.Put (" -i" & Text_Handler.Value (Init_Offset));
  end if;

  if To_Text then
    Ada.Text_Io.Put (" ");
    -- Key coded onto text
    -- Switch and separator
    Ada.Text_Io.Put (Text_Handler.Value (Switch) & Separator);
    for I in 1 .. Text_Handler.Length (Rotors) loop
      if I rem 2 = 1 then
        -- Rotor letter
        declare
          Num : constant Natural
              := Natural'Value (Text_Handler.Value (Rotors)(I) & "");
        begin
          if Num /= 0 then
            Ada.Text_Io.Put (Upper_Str (Num_Letters.Letters_Of (Num)));
          else
            Ada.Text_Io.Put (Upper_Str (Num_Letters.Letters_Of (10)));
          end if;
        end;
        -- Ring offset
        Ada.Text_Io.Put (Text_Handler.Value (Rotors)(I+1));
        -- Initial offset
        Ada.Text_Io.Put (Text_Handler.Value (Init_Offset)((I-1)/2+1));
      end if;
    end loop;
    -- Reflector: Num, offset, offset and num
    declare
      Reflector_Num : constant Positive
               := Positive'Value (Text_Handler.Value (Reflector)(1) & "");

      Reflector_Name : constant String
                := Upper_Str (Num_Letters.Letters_Of (Reflector_Num));
    begin
      Ada.Text_Io.Put (Reflector_Name);
      Ada.Text_Io.Put (Text_Handler.Value (Reflector)(2));
      Ada.Text_Io.Put (Text_Handler.Value (Reflector)(2));
      Ada.Text_Io.Put (Reflector_Name);
    end;
  end if;

  Ada.Text_Io.New_Line;
end Def_Enigma;

