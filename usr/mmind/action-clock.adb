with Chronos, Timers, Normal;
separate (Action)
-- Handle the clock
package body Clock is

  -- The chrono and timer
  Chrono : Chronos.Chrono_Type;
  Timer : Timers.Timer_Id;

  -- The expiration callback
  function Timer_Expire (Unused_Id : in Timers.Timer_Id;
                         Unused_Data : in Timers.Timer_Data) return Boolean is
  begin
    return True;
  end Timer_Expire;

  -- Draw or redrawk clock
  procedure Expire is
    Date : Chronos.Date_Rec;
    Str : Screen.Clock_Str;
  begin
    -- Build clock string and display
    Date := Chrono.Read;
    Str := Normal (Date.Hours,   2, Gap => '0') & ":"
         & Normal (Date.Minutes, 2, Gap => '0') & ":"
         & Normal (Date.Seconds, 2, Gap => '0');
    Screen.Put_Clock (Str);
  end Expire;

  -- Reset and restart the clock
  procedure Start is
  begin
    Timer.Create (
      (Delay_Kind => Timers.Delay_Sec,
       Clock      => null,
       Period     => 1.0,
       Delay_Seconds => 1.0),
      Timer_Expire'Access);
    Chrono.Reset;
    Chrono.Start;
    Expire;
  end Start;

  -- Stop (freeze)
  procedure Stop is
  begin
    Timer.Delete;
    Chrono.Stop;
  end Stop;

end Clock;


