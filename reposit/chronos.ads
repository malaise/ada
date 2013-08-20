-- Simple chronometer that can be start/stop/reset...
-- But also follows change of a virtual time if attached to it
with Ada.Finalization, Ada.Calendar;
with Perpet, Day_Mng, Virtual_Time;
package Chronos is

  type Chrono_Type is new Virtual_Time.Observer with private;

  -- The time read at a chronometer (days and seconds)
  subtype Time_Rec is Perpet.Delta_Rec;
  -- Same but with day duration split
  subtype Day_Range is Perpet.Day_Range;
  type Date_Rec is record
    Days      : Day_Range;
    Hours     : Day_Mng.T_Hours;
    Minutes   : Day_Mng.T_Minutes;
    Seconds   : Day_Mng.T_Seconds;
    Millisecs : Day_Mng.T_Millisec;
  end record;

  -- Status of a chrono
  type Status_List is (Running, Stopped);

  -- Start the chrono
  -- Default status is stopped
  -- No effect if it is already running
  procedure Start (A_Chrono : in out Chrono_Type);

  -- Same and returns current virtual time (when chrono has started)
  procedure Start (A_Chrono : in out Chrono_Type;
                   Vtime : out Virtual_Time.Time);

  -- Stop the chrono
  -- No effect if it is already stopped
  procedure Stop (A_Chrono : in out Chrono_Type);

  -- Get the status of the chrono
  function Get_Status (A_Chrono : Chrono_Type) return Status_List;

  -- Reads the chrono
  -- Chrono can be running or stopped
  function Read (A_Chrono : Chrono_Type) return Time_Rec;
  function Read (A_Chrono : Chrono_Type) return Date_Rec;

  -- Reset the chrono to 0
  -- Does not stop it if it is running (but resets it to 0 anyway)
  procedure Reset (A_Chrono : in out Chrono_Type);

  -- Raised by Read if chrono has reached a too high value
  Time_Error : exception renames Ada.Calendar.Time_Error;


  -- Attach the Chrono to a (new) clock (detach from previous clock if any)
  -- Chrono must be stopped, it is reset
  -- Set A_Clock to null for real time clock
  -- By default, chronos are on real time clock (Clock is null)
  Chrono_Running : exception;
  procedure Attach (A_Chrono : in out Chrono_Type;
                    A_Clock : in Virtual_Time.Clock_Access);

  -- Get the clock attached to the Chrono (null if real time)
  function Get_Clock (A_Chrono : in Chrono_Type)
                     return Virtual_Time.Clock_Access;

private

  type Chrono_Type is new Ada.Finalization.Controlled
                      and Virtual_Time.Observer with record
    -- Chrono status
    Status : Status_List := Stopped;
    -- Time when it was started, when running
    Start_Time : Virtual_Time.Time;
    -- Offset, when stopped or running
    Offset : Duration := 0.0;
    -- Virtual clock (when attached)
    Clock : Virtual_Time.Clock_Access := null;
  end record;

  -- Interface for the virtual clock
  overriding procedure Notify (An_Observer : in out Chrono_Type;
                               Rtime, Vtime : in Virtual_Time.Time;
                               Speed : in Virtual_Time.Speed_Range;
                               A_Clock : in Virtual_Time.Clock_Access);

  overriding procedure Finalize (Chrono : in out Chrono_Type);

end Chronos;

