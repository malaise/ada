-- Simple chronometer that can be start/stop/reset...
with Perpet;
package body Chronos is

  -- Start the chrono
  -- Default status is stopped
  -- No effect if it is already running
  procedure Start (A_Chrono : in out Chrono_Type) is
  begin
    -- Reset start time
    A_Chrono.Start_Time := Ada.Calendar.Clock;
    A_Chrono.Status := Running;
  end Start;

  -- Stop the chrono
  -- No effect if it is already stopped
  procedure Stop (A_Chrono : in out Chrono_Type) is
    use type Ada.Calendar.Time;
  begin
    -- Add current offset to chrono offset
    A_Chrono.Offset := A_Chrono.Offset
                     + Ada.Calendar.Clock
                     - A_Chrono.Start_Time;
    A_Chrono.Status := Stopped;
  end Stop;

  -- Get the status of the chrono
  function Get_Status (A_Chrono : Chrono_Type) return Status_List is
  begin
    return A_Chrono.Status;
  end Get_Status;

  -- Reads the chrono
  -- Chrono can be running or stopped
  function Read (A_Chrono : Chrono_Type) return Time_Rec is
    Curr_Delta : Perpet.Delta_Rec;
    Now : Ada.Calendar.Time;
    Result : Time_Rec;
    use type Perpet.Delta_Rec, Ada.Calendar.Time;
  begin
    Now := Ada.Calendar.Clock;
    if A_Chrono.Status = Running then
      -- Compute delta since chrono started + offset
      Curr_Delta := Perpet.To_Delta_Rec (
           A_Chrono.Offset + Now - A_Chrono.Start_Time);
    else
      -- Just Chrono.Offset
      Curr_Delta := Perpet.To_Delta_Rec (A_Chrono.Offset);
    end if;
    -- Keep days and split seconds
    Result.Days := Curr_Delta.Days;
    Day_Mng.Split (Curr_Delta.Secs,
      Result.Hours, Result.Minutes, Result.Seconds, Result.Millisecs);
    -- Done
    return Result;
  exception
    when others =>
      raise Time_Error;
  end Read;

  -- Reset the chrono
  -- Does not stop it if it is running (but resets it)
  procedure Reset (A_Chrono : in out Chrono_Type) is
  begin
    A_Chrono.Start_Time := Ada.Calendar.Clock;
  end Reset;

end Chronos;

