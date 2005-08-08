with Ada.Calendar, Ada.Text_Io, Ada.Characters.Latin_1;
with Perpet, My_Io;
procedure T_Perpet is

  T : Ada.Calendar.Time;
  R : Perpet.Duration_Rec;
  D  : Perpet.Day_Range;

  procedure Error is
  begin
    My_Io.Put(Ada.Characters.Latin_1.Bel);
    Ada.Text_Io.Skip_Line;
    Ada.Text_Io.Skip_Line;
  end Error;

  function Get return Ada.Calendar.Time is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
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

        return Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
      exception
        when Ada.Calendar.Time_Error => Error;
      end;
    end loop;
  end Get;

  function Get return Perpet.Duration_Rec is
    Dur : Perpet.Duration_Rec;
    use My_Io;
  begin
    loop
      begin
        Put ("Years -> "); Get (Dur.Years);
        exit;
      exception
        when others => Error;
      end;
    end loop;

    loop
      begin
        Put ("Months -> "); Get (Dur.Months);
        exit;
      exception
        when others => Error;
      end;
    end loop;

    return Dur;
  end Get;


  function Get return Perpet.Day_Range is
    D : Perpet.Day_Range;
    use My_Io;
  begin
    loop
      begin
        Put ("Days -> "); Get (D);
        exit;
      exception
        when others => Error;
      end;
    end loop;

    return D;
  end Get;

  procedure Put (Date : in Ada.Calendar.Time) is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    Seconds : Ada.Calendar.Day_Duration;
    use My_Io;
  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Seconds);
    Put (Year); Put (" ");
    Put (Month); Put (" ");
    Put (Day); Put (" ");
    My_Io.New_Line;
  end Put;

begin
  My_Io.Put_Line ("Base :");
  T := Get;
  My_Io.New_Line;
  loop
    begin
      My_Io.Put_Line ("Delta :");
      R := Get;
      My_Io.Put (" Base + Delta:"); Put (Perpet."+"(T, R));
      My_Io.Put (" Base - Delta:"); Put (Perpet."-"(T, R));
      My_Io.New_Line;

      My_Io.Put_Line ("Delta :");
      D := Get;
      My_Io.Put (" Base + Delta:"); Put (Perpet."+"(T, D));
      My_Io.Put (" Base - Delta:"); Put (Perpet."-"(T, D));
      My_Io.New_Line (2);
    exception
      when Ada.Calendar.Time_Error =>
        My_Io.Put_Line ("TIME_ERROR");
    end;
  end loop;
end T_Perpet;
