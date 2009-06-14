with Ada.Text_Io;
with Date_Image;
with Virtual_Time, Chronos, Timers, Event_Mng;
procedure T_Virtual is

  type Observer_Rec is new Virtual_Time.Observer with null record;

  My_Clock : aliased Virtual_Time.Clock;
  My_Observer : aliased Observer_Rec;
  My_Chrono : Chronos.Chrono_Type;
  My_Tid : Timers.Timer_Id;
  pragma Unreferenced (My_Tid);

  procedure Notify (An_Observer : in out Observer_Rec;
                    Rtime, Vtime : in Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    A_Clock : in Virtual_Time.Clock_Access) is
    pragma Unreferenced (An_Observer, Rtime, Speed);
    Rt, Vt : Virtual_Time.Time;
  begin
    Ada.Text_Io.Put_Line ("Observer notified of change done at V "
                          & Date_Image (Vtime));
    A_Clock.Get_Synchro (Rt, Vt);
    Ada.Text_Io.Put_Line ("Synchro is " & Date_Image (Rt)
                        & " " & Date_Image (Vt));
    Ada.Text_Io.Put_Line ("Speed is " & A_Clock.Get_Speed'Img);
  end Notify;

  function Timer_Callback (Id : in Timers.Timer_Id;
                           Data : in Timers.Timer_Data := Timers.No_Data)
           return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Ada.Text_Io.Put_Line ("Timer expiration at "
      & Date_Image (Virtual_Time.Current_Time (null))
      & " - " & Date_Image (My_Clock.Current_Time));
    return False;
  end Timer_Callback;

begin
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Start chrono and timer and waiting 3s");
  My_Chrono.Start;
  My_Tid := Timers.Create (Delay_Spec => (Delay_Kind => Timers.Delay_Sec,
                                          Clock => My_Clock'Unrestricted_Access,
                                          Period => 1.0,
                                          Delay_Seconds => 1.0),
                           Callback => Timer_Callback'Unrestricted_Access);
  Event_Mng.Wait (3_000);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Registering observer");
  My_Clock.Add_Observer (My_Observer'Unrestricted_Access);
  Ada.Text_Io.Put_Line ("Stopping, attaching and starting chrono");
  My_Chrono.Stop;
  My_Chrono.Attach (My_Clock'Unrestricted_Access);
  My_Chrono.Start;
  Ada.Text_Io.Put_Line ("Waiting 1s");
  Event_Mng.Wait (1_000);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Setting speed to 0.5 and waiting 5s");
  My_Clock.Set_Speed (0.5);
  Event_Mng.Wait (5_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Setting speed to 0.0 and waiting 3s");
  My_Clock.Set_Speed (0.0);
  Event_Mng.Wait (3_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Setting speed to 1.0 and waiting 3s");
  My_Clock.Set_Speed (1.0);
  Event_Mng.Wait (3_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Done.");
end T_Virtual;

