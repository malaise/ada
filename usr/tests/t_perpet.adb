with Ada.Calendar;
with Perpet, Basic_Proc, Gets, Console;
procedure T_Perpet is

  T : Ada.Calendar.Time;
  R : Perpet.Duration_Rec;
  D  : Perpet.Day_Range;

  procedure Error is
  begin
    Console.Sound;
  end Error;

  function Get return Ada.Calendar.Time is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    use Basic_Proc;
  begin
    loop
      begin
        loop
          begin
            Put_Output ("Year -> "); Year := Gets.Get_Int (Get_Line);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Month -> "); Month := Gets.Get_Int (Get_Line);
            exit;
          exception
            when others => Error;
          end;
        end loop;

        loop
          begin
            Put_Output ("Day -> "); Day := Gets.Get_Int (Get_Line);
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
    use Basic_Proc;
  begin
    loop
      begin
        Put_Output ("Years -> "); Dur.Years := Gets.Get_Int (Get_Line);
        exit;
      exception
        when Basic_Proc.End_Error => raise;
        when others => Error;
      end;
    end loop;

    loop
      begin
        Put_Output ("Months -> "); Dur.Months := Gets.Get_Int (Get_Line);
        exit;
      exception
        when others => Error;
      end;
    end loop;

    return Dur;
  end Get;


  function Get return Perpet.Day_Range is
    D : Perpet.Day_Range;
    use Basic_Proc;
  begin
    loop
      begin
        Put_Output ("Days -> "); D := Gets.Get_Int (Get_Line);
        exit;
      exception
        when Basic_Proc.End_Error => raise;
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
    use Basic_Proc;
  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Seconds);
    Put_Line_Output (Year'Img & Month'Img & Day'Img);
  end Put;

begin
  Basic_Proc.Put_Line_Output ("Base :");
  T := Get;
  Basic_Proc.New_Line_Output;
  loop
    begin
      Basic_Proc.Put_Line_Output ("Delta :");
      R := Get;
      Basic_Proc.Put_Output (" Base + Delta:"); Put (Perpet."+"(T, R));
      Basic_Proc.Put_Output (" Base - Delta:"); Put (Perpet."-"(T, R));
      Basic_Proc.New_Line_Output;

      Basic_Proc.Put_Line_Output ("Delta :");
      D := Get;
      Basic_Proc.Put_Output (" Base + Delta:"); Put (Perpet."+"(T, D));
      Basic_Proc.Put_Output (" Base - Delta:"); Put (Perpet."-"(T, D));
      Basic_Proc.New_Line_Output;
      Basic_Proc.New_Line_Output;
    exception
      when Ada.Calendar.Time_Error =>
        Basic_Proc.Put_Line_Output ("TIME_ERROR");
    end;
  end loop;
exception
  when Basic_Proc.End_Error => null;
end T_Perpet;

