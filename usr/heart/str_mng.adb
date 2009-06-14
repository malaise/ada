with Perpet, Normal, Language;
package body Str_Mng is

  -- Is the str only spaces
  function Is_Spaces (Str : String) return Boolean is
    Spaces : constant String (Str'Range) := (others => ' ');
  begin
    return Str = Spaces;
  end Is_Spaces;

  -- Has the str some spaces
  function Has_Spaces (Str : String) return Boolean is
  begin
    for I in Str'Range loop
      if Str(I) = ' ' then
        return True;
      end if;
    end loop;
    return False;
  end Has_Spaces;

  -- Parse spaces from a string
  procedure Parse (Str : in out String) is
   F, O : Natural;
   Prev_Space : Boolean;
  begin
    if Is_Spaces (Str) then
      return;
    end if;

    O := Str'First - 1;
    -- Skip heading spaces : F first significant char
    for I in Str'Range loop
      F := I;
      exit when Str(I) /= ' ';
    end loop;

    -- Skip multi spaces between words
    Prev_Space := False;
    for I in F .. Str'Last loop
      if Str(I) /= ' ' then
        Prev_Space := False;
        O := O + 1;
        Str(O) := Str(I);
      else
        if not Prev_Space then
          O := O + 1;
          Str(O) := Str(I);
          Prev_Space := True;
        end if;
      end if;
    end loop;
    -- O index the last significant char or last space
    if Str(O) = ' ' then
      O := O - 1;
    end if;
    -- Fill tail with spaces
    Str (O + 1 .. Str'Last) := (others => ' ');
  end Parse;

  -- True if a parsed string has spaces in the middle
  function Has_Holes (Str : String) return Boolean is
    Space_Found : Boolean;
  begin

    Space_Found := False;
    for I in Str'Range loop
      if Str(I) = ' ' then
        Space_Found := True;
      else
        if Space_Found then
          -- Not a space and a space found previously
          return True;
        end if;
      end if;
    end loop;
    return False;
  end Has_Holes;

  -- 0 <-> spaces
  -- others <-> value
  function To_Str (Bpm : Pers_Def.Bpm_Range) return Bpm_Str is
    use Pers_Def;
  begin
    if Bpm = Pers_Def.Bpm_Range'First then
      return Bpm_Str'(others => ' ');
    else
      -- Align right
      return Normal(Integer(Bpm), Bpm_Str'Length);
    end if;
  end To_Str;

  function To_Bpm (Str : Bpm_Str) return Pers_Def.Bpm_Range is
    Loc_Str : Bpm_Str := Str;
    Result : Pers_Def.Set_Bpm_Range;
  begin
    if Is_Spaces (Loc_Str) then
      return 0;
    end if;
    Parse (Loc_Str);
    Result := Pers_Def.Set_Bpm_Range'Value(Loc_Str);
    return Result;
  end To_Bpm;

  function Pid_Str (Pid : Pers_Def.Pid_Range) return Mesu_Nam.File_Pid_Str is
  begin
    return Normal (Integer(Pid), 3, Gap => '0');
  end Pid_Str;



--  type Date_Str_Rec is record
--    Day : Str2;
--    Month : Str2;
--    Year : Str4;
--  end record;
  -- An input date can be before or after
  -- Check its validity and build date YYyyNnDd
  procedure Check_Date (Input  : in Date_Str_Rec;
                        After  : in Boolean;
                        Output : out Mesu_Def.Date_Str;
                        Valid  : out Boolean) is
    Years  : Ada.Calendar.Year_Number;
    Months : Ada.Calendar.Month_Number;
    Days   : Ada.Calendar.Day_Number;
    Current_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Dummy  : Ada.Calendar.Time;
    pragma Unreferenced (Dummy);
  begin
    -- No space or all spaces in each field
    if      (not Is_Spaces(Input.Day)   and then Has_Spaces(Input.Day))
    or else (not Is_Spaces(Input.Month) and then Has_Spaces(Input.Month))
    or else (not Is_Spaces(Input.Year)  and then Has_Spaces(Input.Year)) then
      Valid := False;
      return;
    end if;
    -- Parse
    if Is_Spaces (Input.Year) then
      -- No year => current year
      Years := Ada.Calendar.Year (Current_Time);
      if Is_Spaces (Input.Month) then
        -- No year no month => current month
        Months := Ada.Calendar.Month(Current_Time);
      else
        -- Month set
        Months := Ada.Calendar.Month_Number'Value(Input.Month);
      end if;
      if Is_Spaces (Input.Day) then
        if Is_Spaces (Input.Month) then
          -- ss ss ssss => current day
          Days := Ada.Calendar.Day (Current_Time);
        else
          -- ss mm ssss => begin/end of month of current year
          if After then
            Days := Perpet.Nb_Days_Month (Years, Months);
          else
            Days := Ada.Calendar.Day_Number'First;
          end if;
        end if;
      else
        -- dd ss ssss  or  dd mm ssss
        Days := Ada.Calendar.Day_Number'Value(Input.Day);
      end if;
    else
      -- Year is set
      Years := Ada.Calendar.Year_Number'Value(Input.Year);
      if Is_Spaces (Input.Month) then
        -- dd ss yyyy forbidden
        if not Is_Spaces (Input.Day) then
          Valid := False;
          return;
        end if;
        -- Year and no month => first/last month
        if After then
          Months := Ada.Calendar.Month_Number'Last;
        else
          Months := Ada.Calendar.Month_Number'First;
        end if;
      else
        -- Month set
        Months := Ada.Calendar.Month_Number'Value(Input.Month);
      end if;
      if Is_Spaces (Input.Day) then
        -- First/last of date
        if After then
          Days := Perpet.Nb_Days_Month (Years, Months);
        else
          Days := Ada.Calendar.Day_Number'First;
        end if;
      else
        -- dd ss ssss  or  dd mm ssss
        Days := Ada.Calendar.Day_Number'Value(Input.Day);
      end if;
    end if;
    -- Does all this stuff make a valid date?
    Dummy := Ada.Calendar.Time_Of (Years, Months, Days);
    Output := Normal (Years,  4, Gap => '0')
            & Normal (Months, 2, Gap => '0')
            & Normal (Days,   2, Gap => '0');
    Valid := True;
  exception
    when others =>
      Valid := False;
  end Check_Date;

  -- Build a rec
  procedure To_Rec (Date : in Mesu_Def.Date_Str;
                    Rec  : out Date_Str_Rec) is
  begin
    Rec.Year  := Date(1 .. 4);
    Rec.Month := Date(5 .. 6);
    Rec.Day   := Date(7 .. 8);
  end To_Rec;

  -- A printed date is Dd/Mm/YYyy
  function To_Printed_Str (Date : Mesu_Def.Date_Str) return Printed_Date_Str is
    S : constant String(1 .. 10) := Date(7 .. 8) & '/' & Date(5 .. 6) & '/' & Date(1 .. 4);
  begin
    return S;
  end To_Printed_Str;

  function To_Date_Str (Printed_Date : Printed_Date_Str)
                        return Mesu_Def.Date_Str is
    S : constant Mesu_Def.Date_Str
      := Printed_Date(7 .. 10) & Printed_Date(4 .. 5) & Printed_Date (1 .. 2);
  begin
    return S;
  end To_Date_Str;

  function Current_Date (Offset : Offset_Range := 0)
  return Mesu_Def.Date_Str is
    Current_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
    Years  : Ada.Calendar.Year_Number;
    Months : Ada.Calendar.Month_Number;
    Days   : Ada.Calendar.Day_Number;
    Secs   : Ada.Calendar.Day_Duration;
    use Perpet;
  begin
    if Offset /= 0 then
      Current_Time := Current_Time - (Years => 0, Months => Offset);
    end if;
    Ada.Calendar.Split (Current_Time, Years, Months, Days, Secs);
    return Normal (Years,  4, Gap => '0')
         & Normal (Months, 2, Gap => '0')
         & Normal (Days,   2, Gap => '0');
  end Current_Date;

  function Current_Date_Printed (Offset : Offset_Range := 0)
  return Printed_Date_Str is
  begin
    return To_Printed_Str (Current_Date(Offset));
  end Current_Date_Printed;

  procedure Current_Date_Rec (Date_Rec : out Date_Str_Rec;
                              Offset : in Offset_Range := 0) is
    Date : constant Mesu_Def.Date_Str := Current_Date (Offset);
  begin
    Date_Rec.Year  := Date(1 .. 4);
    Date_Rec.Month := Date(5 .. 6);
    Date_Rec.Day   := Date(7 .. 8);
  end Current_Date_Rec;

  -- From a person rec to person in list
  procedure Format_Person_To_List (Person    : in Pers_Def.Person_Rec;
                                   List_Pers : out Afpx.Line_Rec) is
  begin
    List_Pers.Str := (others => ' ');
    List_Pers.Len := 32;
    List_Pers.Str (01 .. 20) := Language.String_To_Wide (Person.Name);
    List_Pers.Str (22 .. 31) := Language.String_To_Wide (Person.Activity);
  end Format_Person_To_List;

  procedure Format_List_To_Person (List_Pers : in Afpx.Line_Rec;
                                   Person    : out Pers_Def.Person_Rec) is
  begin
    Person.Name := Language.Wide_To_String (List_Pers.Str (01 .. 20));
    Person.Activity := Language.Wide_To_String (List_Pers.Str (23 .. 32));
  end Format_List_To_Person;


  -- From a mesure rec to mesure in list
  procedure Format_Mesure_To_List (Person    : in Pers_Def.Person_Rec;
                                   Mesure    : in Mesu_Def.Mesure_Rec;
                                   Mesu_No   : in Mesu_Nam.File_No_Str;
                                   List_Mesu : out Afpx.Line_Rec) is
  begin
    Format_Person_To_List (Person, List_Mesu);
    List_Mesu.Len := 71;
    List_Mesu.Str(35 .. 44) := Language.String_To_Wide (To_Printed_Str (Mesure.Date));
    List_Mesu.Str(46 .. 65) := Language.String_To_Wide (Mesure.Comment);
    List_Mesu.Str(67 .. 69) := Language.String_To_Wide (Pid_Str(Person.Pid));
    List_Mesu.Str(70 .. 71) := Language.String_To_Wide (Mesu_No);
  end Format_Mesure_To_List;

  procedure Format_List_To_Mesure (List_Mesu : in Afpx.Line_Rec;
                                   File_Name : out Mesu_Nam.File_Name_Str) is
    Date : constant Printed_Date_Str
         := Language.Wide_To_String (List_Mesu.Str(35 .. 44));
    No   : constant Mesu_Nam.File_No_Str
         := Language.Wide_To_String (List_Mesu.Str(70 .. 71));
    Pid  : constant Mesu_Nam.File_Pid_Str
         := Language.Wide_To_String (List_Mesu.Str(67 .. 69));

  begin
    File_Name :=
     Mesu_Nam.Build_File_Name (Date => To_Date_Str (Date),
                               No   => No,
                               Pid  => Pid);
  end Format_List_To_Mesure;


end Str_Mng;
