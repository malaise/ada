with My_Math;
package body Day_Mng is
  use My_Math;

  Min_In_Hor : constant := 60;
  Sec_In_Min : constant := 60;
  Mil_In_Sec : constant := 1_000;

  -- split a day duration in hours, minutes, seconds and milliseconds
  procedure Split (Dur : in Calendar.Day_Duration;
   Hours    : out T_Hours;
   Minutes  : out T_Minutes;
   Seconds  : out T_Seconds;
   Millisec : out T_Millisec) is
    Rdur : My_Math.Real;
    Sec : My_Math.Real;
    H   : T_Hours;
    M   : T_Minutes;
    S   : T_Seconds;
  begin
    -- Round millisecs
    Rdur := My_Math.Real(My_Math.Round(My_Math.Real(Dur) * My_Math.Real(Mil_In_Sec)))
          / My_Math.Real(Mil_In_Sec);

    -- Millisecs
    Millisec := T_Millisec (My_Math.Frac(Rdur) * My_Math.Real(Mil_In_Sec));

    -- Seconds in the day
    Sec := My_Math.Int (Rdur);

    -- split seconds
    H := T_Hours(My_Math.Trunc(Sec / My_Math.Real(Min_In_Hor * Sec_In_Min) ));
    Sec := Sec - My_Math.Real(H) * My_Math.Real(Min_In_Hor * Sec_In_Min);
    M := T_Minutes(My_Math.Trunc(Sec / My_Math.Real(Sec_In_Min) ));
    Sec := Sec - My_Math.Real(M) * My_Math.Real(Sec_In_Min);
    S := T_Seconds(My_Math.Trunc(Sec) );

    -- out values
    Hours := H;
    Minutes := M;
    Seconds := S;

  end Split;


  -- pack hours, minutes, seconds and milliseconds into a day duration
  function Pack (
   Hours    : T_Hours;
   Minutes  : T_Minutes;
   Seconds  : T_Seconds;
   Millisec : T_Millisec) return Calendar.Day_Duration is
    Dur : Calendar.Day_Duration;
  begin
    Dur := Calendar.Day_Duration (
     Calendar.Day_Duration(Millisec) / Mil_In_Sec);
    Dur := Dur + Calendar.Day_Duration (Seconds);
    Dur := Dur + Calendar.Day_Duration (Minutes * Sec_In_Min);
    Dur := Dur + Calendar.Day_Duration (Hours * Sec_In_Min * Min_In_Hor);
    return Dur;
  end Pack;

end Day_Mng;

