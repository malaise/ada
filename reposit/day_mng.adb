with My_Math;
package body Day_Mng is
  use type My_Math.Real;

  Min_In_Hor : constant := 60;
  Sec_In_Min : constant := 60;
  Mil_In_Sec : constant := 1_000;
  Mic_In_Sec : constant := 1_000_000;

  -- Split a day duration in hours, minutes, seconds and milliseconds
  procedure Split (Dur : in Ada.Calendar.Day_Duration;
                   Hours     : out T_Hours;
                   Minutes   : out T_Minutes;
                   Seconds   : out T_Seconds;
                   Millisecs : out T_Millisecs) is
    Rdur : My_Math.Real;
    Sec : My_Math.Real;
    H   : T_Hours;
    M   : T_Minutes;
    S   : T_Seconds;
  begin
    -- Round millisecs
    Rdur := My_Math.Round_At (My_Math.Real(Dur), -3);

    -- Millisecs
    Millisecs := T_Millisecs (My_Math.Frac(Rdur) * My_Math.Real(Mil_In_Sec));

    -- Seconds in the day
    Sec := My_Math.Int (Rdur);

    -- Split seconds
    H := T_Hours(My_Math.Trunc(Sec / My_Math.Real(Min_In_Hor * Sec_In_Min) ));
    Sec := Sec - My_Math.Real(H) * My_Math.Real(Min_In_Hor * Sec_In_Min);
    M := T_Minutes(My_Math.Trunc(Sec / My_Math.Real(Sec_In_Min) ));
    Sec := Sec - My_Math.Real(M) * My_Math.Real(Sec_In_Min);
    S := T_Seconds(My_Math.Trunc(Sec) );

    -- Out values
    Hours := H;
    Minutes := M;
    Seconds := S;

  end Split;

  -- Split a day duration in hours, minutes, seconds and microsseconds
  procedure Splitu (Dur : in Ada.Calendar.Day_Duration;
                    Hours     : out T_Hours;
                    Minutes   : out T_Minutes;
                    Seconds   : out T_Seconds;
                    Microsecs : out T_Microsecs) is
    Rdur : My_Math.Real;
    Sec : My_Math.Real;
    H   : T_Hours;
    M   : T_Minutes;
    S   : T_Seconds;
  begin
    -- Round microsecs
    Rdur := My_Math.Round_At (My_Math.Real(Dur), -6);

    -- Microsecs
    Microsecs := T_Microsecs (My_Math.Frac(Rdur) * My_Math.Real(Mil_In_Sec));

    -- Seconds in the day
    Sec := My_Math.Int (Rdur);

    -- Split seconds
    H := T_Hours(My_Math.Trunc(Sec / My_Math.Real(Min_In_Hor * Sec_In_Min) ));
    Sec := Sec - My_Math.Real(H) * My_Math.Real(Min_In_Hor * Sec_In_Min);
    M := T_Minutes(My_Math.Trunc(Sec / My_Math.Real(Sec_In_Min) ));
    Sec := Sec - My_Math.Real(M) * My_Math.Real(Sec_In_Min);
    S := T_Seconds(My_Math.Trunc(Sec) );

    -- Out values
    Hours := H;
    Minutes := M;
    Seconds := S;

  end Splitu;

  -- Pack hours, minutes, seconds and milliseconds into a day duration
  function Pack (Hours     : T_Hours;
                 Minutes   : T_Minutes;
                 Seconds   : T_Seconds;
                 Millisecs : T_Millisecs) return Ada.Calendar.Day_Duration is
    Dur : Ada.Calendar.Day_Duration;
  begin
    Dur := Ada.Calendar.Day_Duration (
     Ada.Calendar.Day_Duration(Millisecs) / Mil_In_Sec);
    Dur := Dur + Ada.Calendar.Day_Duration (Seconds);
    Dur := Dur + Ada.Calendar.Day_Duration (Minutes * Sec_In_Min);
    Dur := Dur + Ada.Calendar.Day_Duration (Hours * Sec_In_Min * Min_In_Hor);
    return Dur;
  end Pack;

  -- Pack hours, minutes, seconds and microseconds into a day duration
  function Packu (Hours    : T_Hours;
                  Minutes  : T_Minutes;
                  Seconds  : T_Seconds;
                  Microsecs : T_Microsecs) return Ada.Calendar.Day_Duration is
    Dur : Ada.Calendar.Day_Duration;
  begin
    Dur := Ada.Calendar.Day_Duration (Duration(Microsecs) / Mic_In_Sec);
    Dur := Dur + Ada.Calendar.Day_Duration (Seconds);
    Dur := Dur + Ada.Calendar.Day_Duration (Minutes * Sec_In_Min);
    Dur := Dur + Ada.Calendar.Day_Duration (Hours * Sec_In_Min * Min_In_Hor);
    return Dur;
  end Packu;

  -- Split a time in years, months, days,
  --  hours, minutes, seconds and milliseconds
  procedure Split (Date : in Ada.Calendar.Time;
                   Year      : out T_Years;
                   Month     : out T_Months;
                   Day       : out T_Days;
                   Hours     : out T_Hours;
                   Minutes   : out T_Minutes;
                   Seconds   : out T_Seconds;
                   Millisecs : out T_Millisecs) is
    Dur : Ada.Calendar.Day_Duration;
  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Dur);
    Split (Dur, Hours, Minutes, Seconds, Millisecs);
  end Split;

  -- Split a time in years, months, days,
  --  hours, minutes, seconds and microseconds
  procedure Splitu (Date : in Ada.Calendar.Time;
                    Year      : out T_Years;
                    Month     : out T_Months;
                    Day       : out T_Days;
                    Hours     : out T_Hours;
                    Minutes   : out T_Minutes;
                    Seconds   : out T_Seconds;
                    Microsecs : out T_Microsecs) is
    Dur : Ada.Calendar.Day_Duration;
  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Dur);
    Split (Dur, Hours, Minutes, Seconds, Microsecs);
  end Splitu;

  -- Pack years, months, days, hours, minutes, seconds and milliseconds
  --  into a time
  function Pack (Year      : T_Years;
                 Month     : T_Months;
                 Day       : T_Days;
                 Hours     : T_Hours;
                 Minutes   : T_Minutes;
                 Seconds   : T_Seconds;
                 Millisecs : T_Millisecs) return Ada.Calendar.Time is
    (Ada.Calendar.Time_Of (Year, Month, Day,
                           Pack (Hours, Minutes, Seconds, Millisecs)) );

  function Packu (Year      : T_Years;
                  Month     : T_Months;
                  Day       : T_Days;
                  Hours     : T_Hours;
                  Minutes   : T_Minutes;
                  Seconds   : T_Seconds;
                  Microsecs : T_Microsecs) return Ada.Calendar.Time is
    (Ada.Calendar.Time_Of (Year, Month, Day,
                           Packu (Hours, Minutes, Seconds, Microsecs)) );

end Day_Mng;

