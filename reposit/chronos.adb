package body Chronos is

  -- Get current time of a chrono
  function Current_Time (A_Chrono : Chrono_Type)
                        return Virtual_Time.Time is
  begin
    return Virtual_Time.Current_Time (A_Chrono.Clock);
  end Current_Time;

  -- Clock has changed at Vtime
  overriding procedure Notify (An_Observer : in out Chrono_Type;
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
    Dummy : Virtual_Time.Time;
  begin
    Start (A_Chrono, Dummy);
  end Start;

  -- Same and returns current virtual time (when chrono has started)
  procedure Start (A_Chrono : in out Chrono_Type;
                   Vtime : out Virtual_Time.Time) is
  begin
    if A_Chrono.Status /= Running then
      -- Reset start time from clock
      A_Chrono.Start_Time := Current_Time (A_Chrono);
      A_Chrono.Status := Running;
    end if;
    Vtime := A_Chrono.Start_Time;
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
    use type Perpet.Delta_Rec, Ada.Calendar.Time;
  begin
    return Perpet.To_Delta_Rec (
        if A_Chrono.Status = Running then
          -- Compute delta since chrono started + offset
          A_Chrono.Offset + (Current_Time (A_Chrono) - A_Chrono.Start_Time)
        else
          -- Just Chrono.Offset
          A_Chrono.Offset);
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

  -- INTERNAL: Detach a clock from a chrono
  --  (when setting a new clock or in Finalize)
  procedure Detach_Clock (A_Chrono : in out Chrono_Type) is
    use type Virtual_Time.Clock_Access;
  begin
    if A_Chrono.Clock /= null then
      -- Unregister as observer of the clock
      A_Chrono.Clock.Del_Observer (A_Chrono'Unchecked_Access);
    end if;
    A_Chrono.Clock := null;
  end Detach_Clock;

  -- Attach a virtal clock to the virtual chrono
  procedure Attach (A_Chrono : in out Chrono_Type;
                    A_Clock : in Virtual_Time.Clock_Access) is
    use type Virtual_Time.Clock_Access;
  begin
    if A_Chrono.Get_Status = Chronos.Running  then
      raise Chrono_Running;
    end if;
    A_Chrono.Reset;
    -- Detach from previous clock if any
    Detach_Clock (A_Chrono);
    -- Register as observer of this clock
    if A_Clock /= null then
      -- Set its new clock and register as observer
      A_Chrono.Clock := A_Clock;
      A_Clock.Add_Observer (A_Chrono'Unchecked_Access);
    end if;
  end Attach;

  -- Get the clock attached to the Chrono (null if real time)
  function Get_Clock (A_Chrono : in Chrono_Type)
                     return Virtual_Time.Clock_Access is
  begin
    return A_Chrono.Clock;
  end Get_Clock;

  -- Finalize: Stop and detach from that clock
  overriding procedure Finalize (Chrono : in out Chrono_Type) is
  begin
    Chrono.Stop;
    Detach_Clock (Chrono);
  end Finalize;

end Chronos;

