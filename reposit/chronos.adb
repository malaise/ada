package body Chronos is

  -- Get current time of a chrono
  function Current_Time (A_Chrono : Chrono_Type)
                        return Virtual_Time.Time is
  begin
    return Virtual_Time.Current_Time (A_Chrono.Clock);
  end Current_Time;

  -- Clock has changed at Vtime
  procedure Notify (An_Observer : in out Chrono_Type;
                    Rtime, Vtime : Virtual_Time.Time;
                    Speed : in Virtual_Time.Speed_Range;
                    A_Clock : in Virtual_Time.Clock_Access) is
    pragma Unreferenced (Rtime, Speed);
    use type Virtual_Time.Time;
    Rt, Vt : Virtual_Time.Time;
  begin
    if An_Observer.Status = Running then
      -- Compute Offset when time changed
      An_Observer.Offset := An_Observer.Offset + Vtime - An_Observer.Start_Time;
      -- New Start_Time becomes new Synchro Vtime of clock
      A_Clock.Get_Synchro (Rt, Vt);
      An_Observer.Start_Time := Vt;
    end if;
  end Notify;

  -- Start the chrono
  -- Default status is stopped
  -- No effect if it is already running
  procedure Start (A_Chrono : in out Chrono_Type) is
  begin
    if A_Chrono.Status /= Running then
      -- Reset start time from clock
      A_Chrono.Start_Time := Current_Time (A_Chrono);
      A_Chrono.Status := Running;
    end if;
  end Start;

  -- Stop the chrono
  -- No effect if it is already stopped
  procedure Stop (A_Chrono : in out Chrono_Type) is
    use type Virtual_Time.Time;
  begin
    if A_Chrono.Status /= Stopped then
      -- Add current offset to chrono offset
      A_Chrono.Offset := A_Chrono.Offset
                       + Current_Time (A_Chrono)
                       - A_Chrono.Start_Time;
      A_Chrono.Status := Stopped;
    end if;
  end Stop;

  -- Get the status of the chrono
  function Get_Status (A_Chrono : Chrono_Type) return Status_List is
  begin
    return A_Chrono.Status;
  end Get_Status;

  -- Reads the chrono
  -- Chrono can be running or stopped
  function Read (A_Chrono : Chrono_Type) return Time_Rec is
    Result : Time_Rec;
    use type Perpet.Delta_Rec, Ada.Calendar.Time;
  begin
    if A_Chrono.Status = Running then
      -- Compute delta since chrono started + offset
      Result := Perpet.To_Delta_Rec (
           A_Chrono.Offset
         + (Current_Time (A_Chrono) - A_Chrono.Start_Time) );
    else
      -- Just Chrono.Offset
      Result := Perpet.To_Delta_Rec (A_Chrono.Offset);
    end if;
    return Result;
  exception
    when others =>
      raise Time_Error;
  end Read;

  -- Read the chrono, return days, hours, minutes...
  function Read (A_Chrono : Chrono_Type) return Date_Rec is
    Time : constant Time_Rec := Read (A_Chrono);
    Result : Date_Rec;
  begin
    -- Keep days and split seconds
    Result.Days := Time.Days;
    Day_Mng.Split (Time.Secs,
      Result.Hours, Result.Minutes, Result.Seconds, Result.Millisecs);
    return Result;
  end Read;

  -- Reset the chrono
  -- Does not stop it if it is running (but resets it)
  procedure Reset (A_Chrono : in out Chrono_Type) is
  begin
    A_Chrono.Start_Time := Current_Time (A_Chrono);
    A_Chrono.Offset := 0.0;
  end Reset;

  -- Attach a virtal clock to the virtual chrono
  procedure Attach (A_Chrono : in out Chrono_Type;
                    A_Clock : in Virtual_Time.Clock_Access) is
    use type Chronos.Status_List, Virtual_Time.Clock_Access;
  begin
    if A_Chrono.Get_Status = Chronos.Running  then
      raise Chrono_Running;
    end if;
    -- Detach from previous clock if any
    Detach (A_Chrono);
    -- Set its new clock
    A_Chrono.Clock := A_Clock;
    A_Chrono.Reset;
    -- Register as observer of this clock
    if A_Clock /= null then
      A_Clock.Add_Observer (A_Chrono'Unchecked_Access);
    end if;
  end Attach;

  -- Detach any virtual clock from the chrono
  procedure Detach (A_Chrono : in out Chrono_Type) is
    use type Chronos.Status_List, Virtual_Time.Clock_Access;
  begin
    if A_Chrono.Get_Status = Chronos.Running  then
      raise Chrono_Running;
    end if;
    A_Chrono.Reset;
    if A_Chrono.Clock = null then
      return;
    end if;
    -- Unregister as observer of the clock
    A_Chrono.Clock.Del_Observer (A_Chrono'Unchecked_Access);
    -- No clock
    A_Chrono.Clock := null;
  end Detach;

  -- Finalize: Stop and detach from that clock
  overriding procedure Finalize (Chrono : in out Chrono_Type) is
  begin
    Chrono.Stop;
    Detach (Chrono);
  end Finalize;

end Chronos;

