with Basic_Proc, Date_Image, Virtual_Time, Chronos, Timers, Event_Mng,
     Chronos.Passive_Timers, Queues.Timed;
procedure T_Virtual is

  -- The virtual clock and its observer
  My_Clock : aliased Virtual_Time.Clock;
  type Observer_Rec is new Virtual_Time.Observer with null record;
  procedure Notify (An_Observer : in out Observer_Rec;
                    Rtime, Vtime : in Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    A_Clock : in Virtual_Time.Clock_Access) is
    pragma Unreferenced (An_Observer);
    Rt, Vt : Virtual_Time.Time;
  begin
    Basic_Proc.Put_Line_Output ("Observer notification");
    Basic_Proc.Put_Line_Output (
         "  at R " & Date_Image (Rtime)
       & " - V " & Date_Image (Vtime)
       & " speed" & Speed'Img);
    A_Clock.Get_Synchro (Rt, Vt);
    Basic_Proc.Put_Line_Output ("  Synchro is R " & Date_Image (Rt)
                        & " - V " & Date_Image (Vt));
    Basic_Proc.Put_Line_Output ("  Speed is" & A_Clock.Get_Speed'Img);
  end Notify;
  My_Observer : aliased Observer_Rec;

  -- Now in real time and Vtime
  procedure Put_Now is
  begin
    Basic_Proc.Put_Line_Output (
        "Now is R " & Date_Image (Virtual_Time.Current_Time (null))
      & " - V " & Date_Image (My_Clock.Current_Time));
  end Put_Now;

  -- The timer and its expiration callback
  My_Tid : Timers.Timer_Id;
  function Timer_Callback (Id : Timers.Timer_Id;
                           Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Basic_Proc.Put_Line_Output ("Timer expiration at R "
      & Date_Image (Virtual_Time.Current_Time (null))
      & " - V " & Date_Image (My_Clock.Current_Time));
    return False;
  end Timer_Callback;

  -- The chrono and output of it
  My_Chrono : Chronos.Chrono_Type;
  procedure Put_Chrono is
    use type Virtual_Time.Clock_Access;
  begin
    Basic_Proc.Put_Output ("Chrono is ");
    if My_Chrono.Get_Clock = null then
      Basic_Proc.Put_Output ("R");
    else
      Basic_Proc.Put_Output ("V");
    end if;
    Basic_Proc.Put_Line_Output (My_Chrono.Read.Secs'Img);
  end Put_Chrono;

  -- The passive timer and check of expiration
  My_Pt : Chronos.Passive_Timers.Passive_Timer;
  function Check_Pt (Pt : Chronos.Passive_Timers.Passive_Timer)
           return Boolean is
    Res : Boolean;
  begin
    Basic_Proc.Put_Output ("Passive timer has");
    Res := Pt.Has_Expired;
    if not Res then
      Basic_Proc.Put_Output (" not");
    end if;
    Basic_Proc.Put_Line_Output (" expired");
    return Res;
  end Check_Pt;

  procedure Check_Pt (Pt : in Chronos.Passive_Timers.Passive_Timer) is
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Dummy := Check_Pt (Pt);
  end Check_Pt;

  -- The timed queue and dump of it
  package Int_Queue_Timed is new Queues.Timed (0, Integer);
  My_Queue : Int_Queue_Timed.Timed_Type;
  procedure Dump_Queue is
    Val : Integer;
    Done : Boolean;
  begin
    Basic_Proc.Put_Output ("Queue contained:");
    loop My_Queue.Pop (Val, Done);
      exit when not Done;
      Basic_Proc.Put_Output (Val'Img);
    end loop;
    Basic_Proc.New_Line_Output;
  end Dump_Queue;

begin
  Put_Now;
  Basic_Proc.Put_Line_Output ("Starting real time chrono "
        & "and 2 timers: active at (1, 1) and passive at (5, 5)");
  My_Chrono.Start;
  My_Tid.Create (Delay_Spec => (Delay_Kind => Timers.Delay_Sec,
                                Clock => My_Clock'Unrestricted_Access,
                                Period => 1.0,
                                Delay_Seconds => 1.0),
                           Callback => Timer_Callback'Unrestricted_Access);
  My_Pt.Start ( (Delay_Kind => Timers.Delay_Sec,
                 Clock => My_Clock'Unrestricted_Access,
                 Period => 5.0,
                 Delay_Seconds => 5.0) );
  Basic_Proc.Put_Line_Output ("Attaching queue and waiting 3s");
  My_Queue.Attach (My_Clock'Unrestricted_Access);
  Check_Pt (My_Pt);
  Event_Mng.Wait (3_000);

  Basic_Proc.New_Line_Output;
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  Basic_Proc.Put_Line_Output ("Registering observer");
  My_Clock.Add_Observer (My_Observer'Unrestricted_Access);
  Basic_Proc.Put_Line_Output ("Stopping, attaching and starting chrono");
  My_Chrono.Stop;
  My_Chrono.Attach (My_Clock'Unrestricted_Access);
  My_Chrono.Start;
  Basic_Proc.Put_Line_Output ("Waiting 1s");
  Event_Mng.Wait (1_000);

  Basic_Proc.New_Line_Output;
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  Basic_Proc.Put_Line_Output ("Pushing in Queue 2 3 and 4");
  My_Queue.Push (2, 2.0);
  My_Queue.Push (3, 3.0);
  My_Queue.Push (4, 4.0);
  Basic_Proc.Put_Line_Output ("Setting speed to 0.5 and waiting 5s");
  My_Clock.Set_Speed (0.5);
  Event_Mng.Wait (5_000);
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  My_Queue.Expire;
  Dump_Queue;

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Pushing in Queue 1");
  My_Queue.Push (1, 1.0);
  Basic_Proc.Put_Line_Output ("Setting speed to 0.0 and waiting 3s");
  My_Clock.Set_Speed (0.0);
  Event_Mng.Wait (3_000);
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  My_Queue.Expire;
  Dump_Queue;

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Pushing in Queue 2 3 and 4");
  My_Queue.Push (2, 2.0);
  My_Queue.Push (3, 3.0);
  My_Queue.Push (4, 4.0);
  Basic_Proc.Put_Line_Output ("Setting speed to 1.0 and waiting 3s");
  My_Clock.Set_Speed (1.0);
  Event_Mng.Wait (3_000);
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  My_Queue.Expire;
  Dump_Queue;

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Pushing in Queue 25 and 35");
  My_Queue.Push (25, 25.0);
  My_Queue.Push (35, 35.0);
  Basic_Proc.Put_Line_Output ("Suspending timer and setting speed to 10.0");
  My_Tid.Suspend;
  My_Clock.Set_Speed (10.0);

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Checking passive timer expiration during 3 s");
  for I in 1 .. 3 loop
    Put_Now;
    while Check_Pt (My_Pt) loop
      null;
    end loop;
    Put_Chrono;
    Basic_Proc.Put_Line_Output ("Waiting 1s");
    Event_Mng.Wait (1_000);
  end loop;

  Basic_Proc.New_Line_Output;
  Put_Now;
  Check_Pt (My_Pt);
  Put_Chrono;
  My_Queue.Expire;
  Dump_Queue;

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Done.");
end T_Virtual;

