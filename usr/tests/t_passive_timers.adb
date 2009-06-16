with Ada.Text_Io;
with Passive_Timers, Timers;

procedure T_Passive_Timers is

  procedure Put (Timer : in String; Expired : in Boolean) is
  begin
    Ada.Text_Io.Put (Timer & " has ");
    if not Expired then
      Ada.Text_Io.Put ("not ");
    end if;
    Ada.Text_Io.Put_Line ("Expired");
  end Put;

  T1 : Passive_Timers.Passive_Timer;

begin

  T1.Start ( (Timers.Delay_Sec, null, 10.0, 10.0) );

  declare
    T2 : Passive_Timers.Passive_Timer;
  begin
    T2.Start ( (Timers.Delay_Sec, null, 2.0, 2.0) );
    while not T1.Has_Expired loop
      Put ("T2", T2.Has_Expired);
      delay 1.0;
    end loop;
    Put ("T1", True);
  end;

  T1.Start ( (Timers.Delay_Sec, null, 1.0, 1.0) );
  for I in 1 .. 10 loop
      Put ("T1", T1.Has_Expired);
      delay 0.5;
  end loop;
end T_Passive_Timers;
