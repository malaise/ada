with Ada.Calendar, Ada.Text_Io;
with Perpet, Day_Mng, Normal, My_Io, Argument, Regular_Expressions, Basic_Proc;
procedure T_Delta_Date is

  package Dur_Io is new Ada.Text_Io.Fixed_Io (Ada.Calendar.Day_Duration);

  T1, T2 : Ada.Calendar.Time;
  D : Perpet.Delta_Rec;
  Error_Raised : exception;

  procedure Error is
  begin
    My_Io.Put("Error, invalid value.");
  end Error;

  function Get return Ada.Calendar.Time is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
    use My_Io;
  begin
    loop
      begin
        loop
          begin
            Put ("Year -> "); Get (Year);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Month -> "); Get (Month);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Day -> "); Get (Day);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Hour -> "); Get (Hour);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Minute -> "); Get (Minute);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Second -> "); Get (Second);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put ("Millisec -> "); Get (Millisec);
            exit;
          exception
            when others => Error;
          end;
        end loop;
        return Ada.Calendar.Time_Of (Year, Month, Day,
                   Day_Mng.Pack(Hour, Minute, Second, Millisec));
      exception
        when Ada.Calendar.Time_Error => Error;
      end;
    end loop;
  end Get;

  function Parse (Str : String) return Ada.Calendar.Time is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
    Lstr : constant String (1 .. Str'Length) := Str;
  begin
    if not Regular_Expressions.Match (
       "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}",
       Str, True) then
      raise Error_Raised;
    end if;
    Year := Ada.Calendar.Year_Number'Value (Lstr(1..4));
    Month := Ada.Calendar.Month_Number'Value (Lstr(6..7));
    Day := Ada.Calendar.Day_Number'Value (Lstr(9..10));
    Hour := Day_Mng.T_Hours'Value (Lstr(12..13));
    Minute := Day_Mng.T_Minutes'Value (Lstr(15..16));
    Second := Day_Mng.T_Seconds'Value (Lstr(18..19));
    Millisec := Day_Mng.T_Millisec'Value (Lstr(21..23));
    return Ada.Calendar.Time_Of (Year, Month, Day,
                   Day_Mng.Pack(Hour, Minute, Second, Millisec));
  exception
    when Constraint_Error | Ada.Calendar.Time_Error =>
      raise Error_Raised;
  end Parse;

  procedure Put (Date : in Ada.Calendar.Time) is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    Secs : Ada.Calendar.Day_Duration;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;

    use My_Io;
  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Secs);
    Day_Mng.Split (Secs, Hour, Minute, Second, Millisec);
    Put (Normal(Year, 4, Gap => '0')); Put ("/");
    Put (Normal(Month, 2, Gap => '0')); Put ("/");
    Put (Normal(Day, 2, Gap => '0')); Put (" ");
    Put (Normal(Hour, 2, Gap => '0')); Put (":");
    Put (Normal(Minute, 2, Gap => '0')); Put (":");
    Put (Normal(Second, 2, Gap => '0')); Put (".");
    Put (Normal(Millisec, 3, Gap => '0'));
  end Put;

begin
  if Argument.Get_Nbre_Arg = 2 then
    T1 := Parse (Argument.Get_Parameter (Occurence => 1));
    Put (T1);
    My_Io.Put (" is a ");
    My_Io.Put_Line (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T1)));

    T2 := Parse (Argument.Get_Parameter (Occurence => 2));
    Put (T2);
    My_Io.Put (" is a ");
    My_Io.Put_Line (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T2)));

    D := Perpet."-"(T1, T2);
    My_Io.Put (" Date1 - Date2:");
    My_Io.Put (D.Days); My_Io.Put (" days ");
    Dur_Io.Put (D.Secs); My_Io.Put (" sec");
    return;
  end if;

  My_Io.Put_Line ("Date1 :");
  T1 := Get;
  Put (T1);
  My_Io.Put (" is a ");
  My_Io.Put_Line (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T1)));
  loop
    begin
      My_Io.Put_Line ("Date2 :");
      T2 := Get;
      Put (T2);
      My_Io.Put (" is a ");
      My_Io.Put_Line (Perpet.Day_Of_Week_List'Image(
                Perpet.Get_Day_Of_Week(T2)));
      D := Perpet."-"(T1, T2);
      My_Io.Put (" Date2 - Date1:");
      My_Io.Put (D.Days); My_Io.Put (" days ");
      Dur_Io.Put (D.Secs); My_Io.Put (" sec");

      My_Io.New_Line (2);
    exception
      when Ada.Calendar.Time_Error =>
        My_Io.Put_Line ("TIME_ERROR");
    end;
  end loop;
exception
  when Error_Raised =>
    Error;
    Basic_Proc.Set_Error_Exit_Code;
end T_Delta_Date;
