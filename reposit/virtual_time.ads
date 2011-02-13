-- A virtual clock can be shift from real time and can have different speed
--  from frozen to 128 times faster than real time
-- Virtual clocks can be used for Chronos (and associated passive timers)
--  Timed queues and Timers.
with Ada.Calendar, Ada.Finalization;
with Limited_List;
package Virtual_Time is

  type Clock is tagged limited private;
  type Clock_Access is access constant Clock;

  subtype Time is Ada.Calendar.Time;

  -- Get current time
  function Current_Time (A_Clock : Clock) return Time;
  -- Use Ada.Calendar.Clock if A_Clock is null
  function Current_Time (A_Clock : Clock_Access) return Time;

  -- Set a new synchro point.
  -- The speed of the clock must be 0.0
  --  otherwise the exception Vtime_Error is raised
  procedure Set_Time (A_Clock : in Clock;
                      Reference_Time : in Time;
                      Virtual_Time : in Time);

  -- Get current synchro point
  procedure Get_Synchro (A_Clock : in Clock;
                         Reference_Time : out Time;
                         Virtual_Time : out Time);
  -- Return Ada.Calendar.Clock if A_Clock is null
  procedure Get_Synchro (A_Clock : in Clock_Access;
                         Reference_Time : out Time;
                         Virtual_Time : out Time);

  -- Set a new speed, change synchro point to current time
  type Speed_Range is new Duration range 0.0 .. 128.0;
  procedure Set_Speed (A_Clock : in Clock;
                       Speed : in Speed_Range);

  -- Get current speed
  function Get_Speed (A_Clock : Clock) return Speed_Range;
  -- Return 1.0 if A_Clock is null
  function Get_Speed (A_Clock : Clock_Access) return Speed_Range;


  -- Get Virtual time corresponding to a Reference time
  function Virtual_Time_Of (A_Clock : Clock;
                            Reference_Time : Time) return Time;
  -- Return Reference_Time is A_Clock is null
  function Virtual_Time_Of (A_Clock : Clock_Access;
                            Reference_Time : Time) return Time;

  -- Get Reference time corresponding to a Virtual time
  -- May raise Vtime_Error is current speed is 0.0
  function Reference_Time_Of (A_Clock : Clock;
                              Virtual_Time : Time) return Time;
  -- Return Virtual_Time is A_Clock is null
  function Reference_Time_Of (A_Clock : Clock_Access;
                              Virtual_Time : Time) return Time;

  -- Exception on time operations
  Time_Error : exception renames Ada.Calendar.Time_Error;
  Vtime_Error : exception;

  -- Observers interface
  type Observer is limited interface;
  -- The observer is notified with the clock characteristics before the change
  --  and with the new clock (from which it can get the new characteristics)
  procedure Notify (An_Observer : in out Observer;
                    Prev_Reference_Time : in Time;
                    Prev_Virtual_Time : in Time;
                    Prev_Speed : in Speed_Range;
                    A_Clock : in Clock_Access) is abstract;

  -- Add an observer
  procedure Add_Observer (A_Clock : Clock;
                          An_Observer : access Observer'Class);

  -- Del an observer
  procedure Del_Observer (A_Clock : Clock;
                          An_Observer : access Observer'Class);

private
  Init : constant Time := Ada.Calendar.Clock;

  type Observer_Access is access all Observer'Class;
  procedure Set (To : out Observer_Access; Val : in Observer_Access);
  package List_Mng is new Limited_List (Observer_Access, Set);

  type Clock_Def_Rec is record
    -- Synchro point
    Refe_Time : Time := Init;
    Virt_Time : Time := Init;
    -- Time speed
    Speed : Speed_Range := 1.0;
    -- List of observers
    Observers : List_Mng.List_Type;
  end record;

  type Clock_Def_Access is access Clock_Def_Rec;

  type Clock is limited new Ada.Finalization.Limited_Controlled with record
    Clock_Access : Clock_Def_Access := new Clock_Def_Rec;
  end record;

  overriding procedure Finalize (A_Clock : in out Clock);

end Virtual_Time;

