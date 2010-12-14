with Ada.Text_Io;
with Date_Image, Virtual_Time, Chronos, Timers, Event_Mng, Chronos.Passive_Timers,
     Queues.Timed;
procedure T_Virtual is

  -- The virtual clock and its observer
  My_Clock : aliased Virtual_Time.Clock;
  type Observer_Rec is new Virtual_Time.Observer with null record;
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
  My_Observer : aliased Observer_Rec;

  -- The timer and its expiration callback
  My_Tid : Timers.Timer_Id;
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

  -- Ther chrono
  My_Chrono : Chronos.Chrono_Type;

  -- The passive timer and check of expiration
  My_Pt : Chronos.Passive_Timers.Passive_Timer;
  function Check_Pt (Pt : Chronos.Passive_Timers.Passive_Timer;
                     Name : String := "") return Boolean is
    Res : Boolean;
  begin
    Ada.Text_Io.Put ("Passive timer");
    if Name /= "" then
      Ada.Text_Io.Put (" " & Name);
    end if;
    Ada.Text_Io.Put (" has");
    Res := Pt.Has_Expired;
    if not Res then
      Ada.Text_Io.Put (" not");
    end if;
    Ada.Text_Io.Put_Line (" expired");
    return Res;
  end Check_Pt;

  procedure Check_Pt (Pt : in Chronos.Passive_Timers.Passive_Timer;
                      Name : in String := "") is
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Dummy := Check_Pt (Pt, Name);
  end Check_Pt;

  -- The timed queue and dump
  package Int_Queue_Timed is new Queues.Timed (0, Integer);
  My_Queue : Int_Queue_Timed.Timed_Type;
  procedure Dump_Queue is
    Val : Integer;
    Done : Boolean;
  begin
    Ada.Text_Io.Put ("Queue contains: ");
    loop
      My_Queue.Pop (Val, Done);
      exit when not Done;
      Ada.Text_Io.Put (Val'Img & " ");
    end loop;
    Ada.Text_Io.New_Line;
  end Dump_Queue;

begin
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Ada.Text_Io.Put_Line (
      "Starting chrono and timers, attaching queue and waiting 3s");
  My_Chrono.Start;
  My_Tid := Timers.Create (Delay_Spec => (Delay_Kind => Timers.Delay_Sec,
                                          Clock => My_Clock'Unrestricted_Access,
                                          Period => 1.0,
                                          Delay_Seconds => 1.0),
                           Callback => Timer_Callback'Unrestricted_Access);
  My_Pt.Start ( (Delay_Kind => Timers.Delay_Sec,
                 Clock => My_Clock'Unrestricted_Access,
                 Period => 5.0,
                 Delay_Seconds => 5.0) );
  My_Queue.Attach (My_Clock'Unrestricted_Access);
  Check_Pt (My_Pt);
  Event_Mng.Wait (3_000);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Registering observer");
  My_Clock.Add_Observer (My_Observer'Unrestricted_Access);
  Ada.Text_Io.Put_Line ("Stopping, attaching and starting chrono");
  My_Chrono.Stop;
  My_Chrono.Attach (My_Clock'Unrestricted_Access);
  My_Chrono.Start;
  Ada.Text_Io.Put_Line ("Waiting 1s");
  Event_Mng.Wait (1_000);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
  Ada.Text_Io.Put_Line ("Pushing in Queue 2 3 and 4");
  My_Queue.Push (2, 2.0);
  My_Queue.Push (3, 3.0);
  My_Queue.Push (4, 4.0);
  Ada.Text_Io.Put_Line ("Setting speed to 0.5 and waiting 5s");
  My_Clock.Set_Speed (0.5);
  Event_Mng.Wait (5_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
  My_Queue.Expire;
  Dump_Queue;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Pushing in Queue 1");
  My_Queue.Push (1, 1.0);
  Ada.Text_Io.Put_Line ("Setting speed to 0.0 and waiting 3s");
  My_Clock.Set_Speed (0.0);
  Event_Mng.Wait (3_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
  My_Queue.Expire;
  Dump_Queue;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Pushing in Queue 2 3 and 4");
  My_Queue.Push (2, 2.0);
  My_Queue.Push (3, 3.0);
  My_Queue.Push (4, 4.0);
  Ada.Text_Io.Put_Line ("Setting speed to 1.0 and waiting 3s");
  My_Clock.Set_Speed (1.0);
  Event_Mng.Wait (3_000);
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
  My_Queue.Expire;
  Dump_Queue;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Pushing in Queue 25 and 35");
  My_Queue.Push (25, 25.0);
  My_Queue.Push (35, 35.0);
  Ada.Text_Io.Put_Line ("Suspending timer and setting speed to 10.0");
  Timers.Suspend (My_Tid);
  My_Clock.Set_Speed (10.0);
  for I in 1 .. 3 loop
    Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
    while Check_Pt (My_Pt) loop
      null;
    end loop;
    Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
    Event_Mng.Wait (1_000);
  end loop;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Now is " & Date_Image (My_Clock.Current_Time));
  Check_Pt (My_Pt);
  Ada.Text_Io.Put_Line ("Chrono is " & My_Chrono.Read.Secs'Img);
  My_Queue.Expire;
  Dump_Queue;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Done.");
end T_Virtual;

