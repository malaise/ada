with Ada.Calendar;
with Basic_Proc, As.B, Perpet, Argument,Normal, Mixed_Str;
procedure Day_Of_Week is

  Day   : Ada.Calendar.Day_Number;
  Month : Ada.Calendar.Month_Number;
  Year  : Ada.Calendar.Year_Number;
  -- dd/mm/yyyy
  Txt : As.B.Asb_Bs(10);
  -- "is", "was" or "will be"
  Verb : As.B.Asb_Bs(7);
  Today, T : Ada.Calendar.Time;
  -- From 1st Jan to today and from today to 31th Dec
  Delta_Date_0 : Perpet.Delta_Rec;
  Delta_Date_1 : Perpet.Delta_Rec;
  Day_No : Perpet.Day_Range;
  Th : String(1 .. 2);
  Days : As.B.Asb_Bs(4);

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Syntax error or invalid date.");
    Basic_Proc.Put_Line_Error (" Usage: "
                 & Argument.Get_Program_Name & " [ dd/mm/yyyy ]");
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

begin
  -- Build time of 00h00 of today
  Today := Ada.Calendar.Clock;
  declare
    Dummy_Duration : Ada.Calendar.Day_Duration;
  begin
    Ada.Calendar.Split (Today, Year, Month, Day, Dummy_Duration);
    Today := Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
  end;

  -- Get date from argument or current date, set T and (Day, Month and Year)
  if Argument.Get_Nbre_Arg = 0 then
    -- Day, Month and Year already set when computing Today
    T := Today;
  elsif Argument.Get_Nbre_Arg = 1 then
    -- Get date from arg 1
    begin
      Txt.Set (Argument.Get_Parameter);
    exception
      when As.B.Length_Error =>
        -- Argument too long
        Usage;
        return;
    end;
    -- Sanity checks of input date
    if Txt.Length /= 10
    or else Txt.Element (3) /= '/'
    or else Txt.Element (6) /= '/' then
      Usage;
      return;
    end if;
    if not Is_Digit (Txt.Slice (1, 2))
    or else not Is_Digit (Txt.Slice (4, 5))
    or else not Is_Digit (Txt.Slice (7, 10)) then
      Usage;
      return;
    end if;
    -- Convert input into Day, Month and Year
    -- and build time of 00h00 of date
    begin
      Day   := Ada.Calendar.Day_Number'Value   (Txt.Slice (1, 2));
      Month := Ada.Calendar.Month_Number'Value (Txt.Slice (4, 5));
      Year  := Ada.Calendar.Year_Number'Value  (Txt.Slice (7, 10));
      T := Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
    exception
      when others =>
        Usage;
        return;
    end;
  else
    Usage;
    return;
  end if;

  -- Text format of date
  Txt.Set (Normal (Day,   2, Gap => '0') & "/"
         & Normal (Month, 2, Gap => '0') & "/"
         & Normal (Year,  4, Gap => '0') );


  -- Compute verb
  declare
    use type Ada.Calendar.Time;
  begin
    if T < Today then
      Verb.Set ("was");
    elsif T = Today then
      Verb.Set ("is");
    else
      Verb.Set ("will be");
    end if;
  end;

  -- Compute delta from 01/01 and to 31/12 of year
  declare
    T0 : Ada.Calendar.Time;
    T1 : Ada.Calendar.Time;
  begin
    T0 := Ada.Calendar.Time_Of (Year, 1, 1, 0.0);
    Delta_Date_0 := Perpet."-" (T, T0);

    T1 := Ada.Calendar.Time_Of (Year, 12, 31, 0.0);
    Delta_Date_1 := Perpet."-" (T1, T);
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Internal error");
      raise;
  end;

  -- Compute sentence
  Day_No := Delta_Date_0.Days + 1;
  if (Day_No rem 100) / 10 /= 1 then
    -- 1..9 and 20..99
    case Day_No rem 10 is
      when 1 =>
        Th := "st";
      when 2 =>
        Th := "nd";
      when 3 =>
        Th := "rd";
      when others =>
        Th := "th";
    end case;
  else
    -- 10..19
    Th := "th";
  end if;

  if Delta_Date_1.Days <= 1 then
    Days.Set ("day");
  else
    Days.Set ("days");
  end if;

  -- Display result
  Basic_Proc.Put_Line_Output (Txt.Image
       & " "
       & Verb.Image
       & " "
       & Mixed_Str (Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T)))
       & " "
       & Normal (Day, 2, Gap => '0')
       & " "
       & Mixed_Str (Perpet.Month_Name_List'Image(Perpet.Get_Month_Name (Month)))
       & " "
       & Normal (Year, 4, Gap => '0')
       & ", in week"
       & Perpet.Week_Of_Year_Range'Image (Perpet.Get_Week_Of_Year (T))
       & ",");
  Basic_Proc.Put_Line_Output (" the"
       & Perpet.Day_Range'Image (Day_No)
       & Th
       & " day of the year,"
       & Perpet.Day_Range'Image (Delta_Date_1.Days)
       & " " & Days.Image
       & " remaining.");

end Day_Of_Week;

