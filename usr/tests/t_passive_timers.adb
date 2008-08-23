with Ada.Text_Io;
with Passive_Timers;

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

  T1.Arm (10.0);

  declare
    T2 : Passive_Timers.Passive_Timer;
  begin
    T2.Arm (2.0);
    while not T1.Has_Expired loop
      Put ("T2", T2.Has_Expired);
      delay 1.0;
    end loop;
    Put ("T1", True);
  end;

  T1.Arm (1.0);
  for I in 1 .. 10 loop
      Put ("T1", T1.Has_Expired);
      delay 0.5;
  end loop;
end T_Passive_Timers;
