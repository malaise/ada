with Ada.Calendar, Ada.Text_Io;
with Perpet, Argument, Day_Mng, Text_Handler, Normal, Mixed_Str;
procedure Day_Of_Week is

  package Dur_Io is new Ada.Text_Io.Fixed_Io (Ada.Calendar.Day_Duration);

  Day   : Ada.Calendar.Day_Number;
  Month : Ada.Calendar.Month_Number;
  Year  : Ada.Calendar.Year_Number;
  Txt : Text_Handler.Text (10);
  T : Ada.Calendar.Time;
  Delta_Date_0 : Perpet.Delta_Rec;
  Delta_Date_1 : Perpet.Delta_Rec;
  Day_No : Perpet.Day_Range;
  Th : String (1 .. 2);
  Days : Text_Handler.Text (4);

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("Syntax error or invalid date.");
    Ada.Text_Io.Put_Line(" Usage: "
                 & Argument.Get_Program_Name
                 & " [ dd/mm/yyyy ]");
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
  else
    Usage;
    return;
  end if;

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
      Ada.Text_Io.Put_Line ("Internal error");
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
    Text_Handler.Set (Days, "day");
  else
    Text_Handler.Set (Days, "days");
  end if;

  -- Display result
  Ada.Text_Io.Put_Line (Text_Handler.Value (Txt) & " is "
       & Mixed_Str (Perpet.Day_Of_Week_List'Image (Perpet.Get_Day_Of_Week (T)))
       & " "
       & Normal (Day, 2, Gap => '0')
       & " "
       & Mixed_Str (Perpet.Month_Name_List'Image(Perpet.Get_Month_Name (Month)))
       & ", in week"
       & Perpet.Week_Of_Year_Range'Image (Perpet.Get_Week_Of_Year (T))
       & ",");
  Ada.Text_Io.Put_Line (" the"
       & Perpet.Day_Range'Image (Day_No)
       & Th
       & " day of the year,"
       & Perpet.Day_Range'Image (Delta_Date_1.Days)
       & " " & Text_Handler.Value (Days)
       & " remaining.");

end Day_Of_Week;

