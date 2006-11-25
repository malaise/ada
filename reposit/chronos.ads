-- Simple chronometer that can be start/stop/reset...
with Ada.Calendar;
with Day_Mng;
package Chronos is

  -- A chronometer
  type Chrono_Type is private;

  -- The time read at a chronometer
  type Time_Rec is record
    Days      : Natural;
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

  -- Stop the chrono
  -- No effect if it is already stopped
  procedure Stop (A_Chrono : in out Chrono_Type);

  -- Get the status of the chrono
  function Get_Status (A_Chrono : Chrono_Type) return Status_List;

  -- Reads the chrono
  -- Chrono can be running or stopped
  function Read (A_Chrono : Chrono_Type) return Time_Rec;

  -- Reset the chrono
  -- Does not stop it if it is running (but resets it)
  procedure Reset (A_Chrono : in out Chrono_Type);

  -- Raised by Read if chrono has reached a too high value
  Time_Error : exception renames Ada.Calendar.Time_Error;

private

  type Chrono_Type is record
    -- Chrono status
    Status : Status_List := Stopped;
    -- Time when it was started, when running
    Start_Time : Ada.Calendar.Time;
    -- Offset, when stopped or running
    Offset : Duration := 0.0; 
  end record;

end Chronos;

