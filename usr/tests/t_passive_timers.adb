with Basic_Proc, Chronos.Passive_Timers, Timers;

procedure T_Passive_Timers is

  procedure Put (Timer : in String; Expired : in Boolean) is
  begin
    Basic_Proc.Put_Output (Timer & " has ");
    if not Expired then
      Basic_Proc.Put_Output ("not ");
    end if;
    Basic_Proc.Put_Line_Output ("Expired");
  end Put;

  T1 : Chronos.Passive_Timers.Passive_Timer;

begin

  Basic_Proc.Put_Line_Output ("Starting T1(10, 10) and T2(2, 2)");
  Basic_Proc.Put_Line_Output (" and checking each sec until T1 expiration");

  T1.Start ( (Timers.Delay_Sec, null, 10.0, 10.0) );

  declare
    T2 : Chronos.Passive_Timers.Passive_Timer;
  begin
    T2.Start ( (Timers.Delay_Sec, null, 2.0, 2.0) );
    while not T1.Has_Expired loop
      Put ("T1", False);
      Put ("T2", T2.Has_Expired);
      delay 1.0;
    end loop;
    Put ("T1", True);
  end;

  Basic_Proc.Put_Line_Output (
      "Starting T1(1, 1) and checking 5 times each 0.5s");
  T1.Start ( (Timers.Delay_Sec, null, 1.0, 1.0) );
  for I in 1 .. 5 loop
      Put ("T1", T1.Has_Expired);
      delay 0.5;
  end loop;

  -- Test Timer_Expired
  declare
    T3 : Chronos.Passive_Timers.Passive_Timer;
  begin
    Basic_Proc.Put_Line_Output (
        "Starting T3(0, 1) and checking after 0.5 then 1.0");
    T3.Start ( (Timers.Delay_Sec, null, 0.0, 1.0) );
    delay 0.5;
    Put ("T3", T3.Has_Expired);
    delay 1.0;
    Put ("T3", T3.Has_Expired);
    Put ("T3", T3.Has_Expired);
  exception
    when Chronos.Passive_Timers.Timer_Expired =>
      Basic_Proc.Put_Line_Output ("Exception Timer_Expired (OK)");
      T3.Stop;
  end;

end T_Passive_Timers;

