-- Simple chronometer that can be start/stop/reset...
package body Chronos is

  -- Start the chrono
  -- Default status is stopped
  -- No effect if it is already running
  procedure Start (A_Chrono : in out Chrono_Type) is
  begin
    if A_Chrono.Status /= Running then
      -- Reset start time
      A_Chrono.Start_Time := Ada.Calendar.Clock;
      A_Chrono.Status := Running;
    end if;
  end Start;

  -- Stop the chrono
  -- No effect if it is already stopped
  procedure Stop (A_Chrono : in out Chrono_Type) is
    use type Ada.Calendar.Time;
  begin
    if A_Chrono.Status /= Stopped then
      -- Add current offset to chrono offset
      A_Chrono.Offset := A_Chrono.Offset
                       + Ada.Calendar.Clock
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
    Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Result : Time_Rec;
    use type Perpet.Delta_Rec, Ada.Calendar.Time;
  begin
    if A_Chrono.Status = Running then
      -- Compute delta since chrono started + offset
      Result := Perpet.To_Delta_Rec (
           A_Chrono.Offset + Now - A_Chrono.Start_Time);
    else
      -- Just Chrono.Offset
      Result := Perpet.To_Delta_Rec (A_Chrono.Offset);
    end if;
    return Result;
  exception
    when others =>
      raise Time_Error;
  end Read;

  function Read (A_Chrono : Chrono_Type) return Date_Rec is
    Time : Time_Rec := Read (A_Chrono);
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
    A_Chrono.Start_Time := Ada.Calendar.Clock;
    A_Chrono.Offset := 0.0;
  end Reset;

end Chronos;

