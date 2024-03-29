-- Test Perpet delta between 2 dates, either provided as argument
--  or inpout interactively
with Ada.Calendar;
with Perpet, Day_Mng, Normal, Basic_Proc, Argument, Reg_Exp,
     Gets, Images;
procedure T_Delta_Date is

  T1, T2 : Ada.Calendar.Time;
  D : Perpet.Delta_Rec;
  Error_Raised : exception;

  procedure Error is
  begin
    Basic_Proc.Put_Output("Error, invalid value.");
  end Error;

  procedure Get (N : out Natural) is
  begin
    N := Gets.Get_Int (Basic_Proc.Get_Line);
  end Get;

  function Get return Ada.Calendar.Time is
    Year : Day_Mng.T_Years;
    Month : Day_Mng.T_Months;
    Day : Day_Mng.T_Days;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisecs;
    use Basic_Proc;
  begin
    loop
      begin
        loop
          begin
            Put_Output ("Year -> "); Get (Year);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Month -> "); Get (Month);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Day -> "); Get (Day);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Hour -> "); Get (Hour);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Minute -> "); Get (Minute);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Second -> "); Get (Second);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Millisec -> "); Get (Millisec);
            exit;
          exception
            when others => Error;
          end;
        end loop;
        return Day_Mng.Pack (Year, Month, Day,
                             Hour, Minute, Second, Millisec);
      exception
        when Ada.Calendar.Time_Error => Error;
      end;
    end loop;
  end Get;

  function Parse (Str : String) return Ada.Calendar.Time is
    Year : Day_Mng.T_Years;
    Month : Day_Mng.T_Months;
    Day : Day_Mng.T_Days;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisecs;
    Lstr : constant String (1 .. Str'Length) := Str;
  begin
    if not Reg_Exp.Match (
       "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}",
       Str, True) then
      raise Error_Raised;
    end if;
    Year := Day_Mng.T_Years'Value (Lstr(1..4));
    Month := Day_Mng.T_Months'Value (Lstr(6..7));
    Day := Day_Mng.T_Days'Value (Lstr(9..10));
    Hour := Day_Mng.T_Hours'Value (Lstr(12..13));
    Minute := Day_Mng.T_Minutes'Value (Lstr(15..16));
    Second := Day_Mng.T_Seconds'Value (Lstr(18..19));
    Millisec := Day_Mng.T_Millisecs'Value (Lstr(21..23));
    return Day_Mng.Pack (Year, Month, Day,
                         Hour, Minute, Second, Millisec);
  exception
    when Constraint_Error | Ada.Calendar.Time_Error =>
      raise Error_Raised;
  end Parse;

  procedure Put (Date : in Ada.Calendar.Time) is
    Year : Day_Mng.T_Years;
    Month : Day_Mng.T_Months;
    Day : Day_Mng.T_Days;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisecs;

    use Basic_Proc;
  begin
    Day_Mng.Split (Date, Year, Month, Day, Hour, Minute, Second, Millisec);
    Put_Output (Normal(Year, 4, Gap => '0')); Put_Output ("/");
    Put_Output (Normal(Month, 2, Gap => '0')); Put_Output ("/");
    Put_Output (Normal(Day, 2, Gap => '0')); Put_Output (" ");
    Put_Output (Normal(Hour, 2, Gap => '0')); Put_Output (":");
    Put_Output (Normal(Minute, 2, Gap => '0')); Put_Output (":");
    Put_Output (Normal(Second, 2, Gap => '0')); Put_Output (".");
    Put_Output (Normal(Millisec, 3, Gap => '0'));
  end Put;

  procedure Put (D : Ada.Calendar.Day_Duration) is
  begin
    Basic_Proc.Put_Output (Images.Dur_Image (D, 3, False));
  end Put;

  use Basic_Proc;
begin
  if Argument.Get_Nbre_Arg = 2 then
    T1 := Parse (Argument.Get_Parameter (Occurence => 1));
    Put (T1);
    Put_Output (" is a ");
    Put_Line_Output (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T1)));

    T2 := Parse (Argument.Get_Parameter (Occurence => 2));
    Put (T2);
    Put_Output (" is a ");
    Put_Line_Output (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T2)));

    D := Perpet."-"(T1, T2);
    Put_Output (" Date1 - Date2: ");
    Put_Output (Images.Integer_Image(D.Days)); Put_Output (" days ");
    Put (D.Secs); Put_Output (" sec");
    New_Line_Output;
    return;
  end if;

  Put_Line_Output ("Date1 :");
  T1 := Get;
  Put (T1);
  Put_Output (" is a ");
  Put_Line_Output (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T1)));
  loop
    begin
      Put_Line_Output ("Date2 :");
      T2 := Get;
      Put (T2);
      Put_Output (" is a ");
      Put_Line_Output (Perpet.Day_Of_Week_List'Image(
                Perpet.Get_Day_Of_Week(T2)));
      D := Perpet."-"(T1, T2);
      Put_Output (" Date2 - Date1: ");
      Put_Output (Images.Integer_Image (D.Days)); Put_Output (" days ");
      Put (D.Secs); Put_Output (" sec");

      Basic_Proc.New_Line_Output;
      Basic_Proc.New_Line_Output;
    exception
      when Ada.Calendar.Time_Error =>
        Put_Line_Output ("TIME_ERROR");
    end;
  end loop;
exception
  when Error_Raised =>
    Error;
    Basic_Proc.Set_Error_Exit_Code;
end T_Delta_Date;

