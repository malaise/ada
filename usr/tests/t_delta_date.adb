with Calendar, Text_Io;
with Perpet, Day_Mng, Normal, My_Io;
procedure T_Delta_Date is

  package Dur_Io is new Text_Io.Fixed_Io (Calendar.Day_Duration);

  T1, T2 : Calendar.Time;
  D : Perpet.Delta_Rec;

  procedure Error is
  begin
    My_Io.Put(Ascii.Bel);
    Text_Io.Skip_Line;
    Text_Io.Skip_Line;
  end Error;

  function Get return Calendar.Time is
    Year : Calendar.Year_Number;
    Month : Calendar.Month_Number;
    Day : Calendar.Day_Number;
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
        return Calendar.Time_Of (Year, Month, Day, Day_Mng.Pack(Hour, Minute, Second, Millisec));
      exception
        when Calendar.Time_Error => Error;
      end;
    end loop;
  end Get;

  procedure Put (Date : in Calendar.Time) is
    Year : Calendar.Year_Number;
    Month : Calendar.Month_Number;
    Day : Calendar.Day_Number;
    Secs : Calendar.Day_Duration;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;

    use My_Io;
  begin
    Calendar.Split (Date, Year, Month, Day, Secs);
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
      My_Io.Put_Line (Perpet.Day_Of_Week_List'Image(Perpet.Get_Day_Of_Week(T2)));
      D := Perpet."-"(T1, T2);
      My_Io.Put (" Date2 - Date1:");
      My_Io.Put (D.Days); My_Io.Put (" days ");
      Dur_Io.Put (D.Secs); My_Io.Put (" sec");
    
      My_Io.New_Line (2);
    exception
      when Calendar.Time_Error =>
        My_Io.Put_Line ("TIME_ERROR");
    end;
  end loop;
end T_Delta_Date;
