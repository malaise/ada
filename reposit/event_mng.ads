with Sys_Calls;
package Event_Mng is

  -------------------
  -- Fd management --
  -------------------
  -- File descriptor
  subtype File_Desc is Sys_Calls.File_Desc;

  -- Each callback is called with the fd and indication of read/write event
  -- and should return True if fd event has to be reported by Wait
  type Fd_Callback is access
    function (Fd : in File_Desc; Read : in Boolean) return Boolean;

  -- Register a callback on a fd for read or write.
  -- The callback will be called (within Wait) with the fd and read
  procedure Add_Fd_Callback (Fd : in File_Desc; Read : in Boolean;
                             Callback : in Fd_Callback);
  -- Unregister the callback from a fd in a mode
  procedure Del_Fd_Callback (Fd : in File_Desc; Read : in Boolean);
  -- Is a callback registered on a fd in a mode
  function Fd_Callback_Set (Fd : in File_Desc; Read : in Boolean)
  return Boolean;

  -----------------------
  -- Signal management --
  -----------------------
  -- Called on some signal: SigInt SigChld
  type Sig_Callback is access procedure;

  -- Register a callback on signal
  -- Wait will generate Signal_Event as soon as there is a callback, so:
  --   Unregister with null, then no Signal_Event will be returned,
  --   Register an empty procedure for the event only. This is the default.
  procedure Set_Sig_Term_Callback (Callback : in Sig_Callback);
  procedure Set_Sig_Child_Callback (Callback : in Sig_Callback);

  -- Is a callback set on signals
  function Sig_Term_Callback_Set return Boolean;
  function Sig_Child_Callback_Set return Boolean;

  -- Return current callback on signal
  function  Get_Sig_Term_Callback return Sig_Callback;
  function  Get_Sig_Child_Callback return Sig_Callback;

  -- Send a dummy signal
  -- It always generates a Sig_Event but Callbacks are not called
  -- Usefull to unblock a Wait or Pause
  procedure Send_Dummy_Signal;


  -- Activate signal handling (capability to catch SigTerm (and Sigint) and
  --  SigChild and report them
  procedure Activate_Signal_Handling;

  -- Reset signal handling to default UNIX behaviour
  -- Returns True if a Sig Term was received but not handled
  function Reset_Default_Signals_Policy return Boolean;

  -------------------
  -- Waiting point --
  -------------------
  -- Wait until some timer or fd callback return True,
  --      until a Dummy_Sig
  --      until a Terminate_Sig or Child_Sig while Sig_Callback set
  --   or until timeout
  -- Any negative timeout means infinite
  -- The three operation end on event or timeout
  Infinite_Ms : constant Integer := -1;
  type Out_Event_List is (Timer_Event, Fd_Event, Signal_Event, No_Event);
  function Wait (Timeout_Ms : Integer) return Out_Event_List;
  function Wait (Timeout_Ms : Integer) return Boolean;
  procedure Wait (Timeout_Ms : Integer);


  -- Waits for the specified delay or a signal
  -- Pause returns if either:
  --  - A signal (even Dummy) is received
  --  - Another Pause (armed earlier) expires (and we are in a Cb of this Pause)
  -- Usefull to wait a bit and still process timers and Fds transparently
  procedure Pause (Timeout_Ms : in Integer);

  ----------------------
  -- Event management --
  ----------------------
  -- This low level operation shall not be used by applications
  -- Event got by another waiting point (X_Wait_Event?)
  subtype In_Event_List is Out_Event_List range Fd_Event .. No_Event;
  type Event_Rec (Kind : In_Event_List := Fd_Event) is record
    case Kind is
      when Fd_Event =>
        Fd : File_Desc;
        Read : Boolean;
      when Signal_Event =>
        null;
      when No_Event =>
        null;
    end case;
  end record;

  -- Handle an event
  function Handle (Event : Event_Rec) return Out_Event_List;

  ----------------
  -- Exceptions --
  ----------------
  Event_Failure : exception;

end Event_Mng;

