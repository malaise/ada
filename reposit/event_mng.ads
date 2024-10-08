-- Event (on fd, timer, signal...) management
with Sys_Calls, Timers;
private with Trace;
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
  -- Raises Fd_Cb_Error if this Fd has already a Callback
  procedure Add_Fd_Callback (Fd : in File_Desc; Read : in Boolean;
                             Callback : in Fd_Callback);
  -- Get current callback on a fd for read or write.
  -- Returns null if no Callback is set for this Fd on this mode
  function Get_Fd_Callback (Fd : in File_Desc; Read : in Boolean)
           return Fd_Callback;
  -- Unregister the callback from a fd in a mode
  -- Raises Fd_Cb_Error if this Fd has no Callback
  procedure Del_Fd_Callback (Fd : in File_Desc; Read : in Boolean);
  -- Is a callback registered on a fd in a mode
  function Fd_Callback_Set (Fd : in File_Desc; Read : in Boolean)
                           return Boolean;

  -----------------------
  -- Signal management --
  -----------------------
  -- Called on some signal: SigTerm (or SigInt), or SigChld
  type Sig_Callback is access procedure;

  -- Register a callback on signal
  -- Wait will generate Signal_Event as soon as there is a callback, so:
  --   Unregister with null, then no Signal_Event will be returned,
  --   Register an empty procedure for only the generation of the event.
  -- By default (at init), signal policy is the UNIX default behaviour
  --  (see Reset_Default_Signals_Policy), but setting any of these
  --  callbacks or calling Wait or Pause first initializes all the callbacks
  --  to ignore (Nul_Procedure)
  procedure Set_Sig_Term_Callback (Callback : in Sig_Callback);
  procedure Set_Sig_Child_Callback (Callback : in Sig_Callback);
  procedure Set_Sig_Usr1_Callback (Callback : in Sig_Callback);
  procedure Set_Sig_Usr2_Callback (Callback : in Sig_Callback);

  -- Is a callback set on signals
  function Sig_Term_Callback_Set return Boolean;
  function Sig_Child_Callback_Set return Boolean;
  function Sig_Usr1_Callback_Set return Boolean;
  function Sig_Usr2_Callback_Set return Boolean;

  -- Return current callback on signal
  function Get_Sig_Term_Callback return Sig_Callback;
  function Get_Sig_Child_Callback return Sig_Callback;
  function Get_Sig_Usr1_Callback return Sig_Callback;
  function Get_Sig_Usr2_Callback return Sig_Callback;

  -- Send a dummy signal
  -- It always generates a Sig_Event but no Callback is called
  -- Usefull to unblock a Wait or Pause
  procedure Send_Dummy_Signal;


  -- Signal handling is the capability to catch SigTerm (and SigInt),
  --  SigChild and dummy signal, and report them
  -- This is automatically activated when setting a signal callback,
  --  when calling Wait (or Pause) or when sending a dummy signal
  -- Non-interactive programs and stand-alone libraries shall:
  --  - Check Are_Signals_Handled and store the result
  --  - Wait and catch signals...
  --  - If signals were not handled, then Reset_Default_Signals_Policy

  -- Reset signal handling to the default UNIX behaviour
  -- Returns True if a Sig Term was received but not handled
  function Reset_Default_Signals_Policy return Boolean;
  -- Reset signal handling to the default UNIX behaviour
  -- Kill ourself SIGTERM if a Sig Term was received but not handled
  procedure Reset_Default_Signals_Policy;

  -- Is signal handling activated
  function Are_Signals_Handled return Boolean;

  -------------------
  -- Waiting point --
  -------------------
  -- WARNING: X11 programs shall use the X waiting point
  --   (X_Wait_Event/Put_Then_Get) instead. If they really need to use Wait,
  --   they shall Suspend ALL the X objects (X_Line/Con_Io/Afpx) before calling
  --   Wait, then call Wait, then Resume the X objects.
  -- WARNING: This opeation is supposed to be called only by the main task.
  --          It shall NOT be called by tasks (use delay).
  --
  -- Wait until a Sig_Term or Child_Sig with a callback set,
  --   or until a Dummy_Sig,
  --   or until some timer expires and its callback returns True,
  --   or until some fd event occur and its callback returns True,
  --   or until timeout
  -- Any negative timeout means infinite
  type Out_Event_List is (Timer_Event, Fd_Event, Signal_Event, Timeout);

  -- Used internally in Handling child pacakge
  subtype In_Event_List is Out_Event_List range Fd_Event .. Timeout;

  -- The 4 variants return on any event or on timeout
  -- This one uses virtual time and allows various specifications of delay
  --  (in real time)
  function Wait (Delay_Spec : Timers.Delay_Rec) return Out_Event_List;

  -- These ones are in milliseconds of real time
  Infinite_Ms : constant Integer := -1;
  function Wait (Timeout_Ms : Integer) return Out_Event_List;
  -- The Boolean is True if an event and False if Timeout
  function Wait (Timeout_Ms : Integer) return Boolean;
  procedure Wait (Timeout_Ms : Integer);


  -- Waits for the specified timeout (in milliseconds of real time) or a signal
  -- Pause returns if either:
  --  - Expiration of the timeout
  --  - A signal (even Dummy) is received
  --  - Another Pause (armed earlier) expires (and we are in a Cb of this Pause)
  -- Usefull to wait a bit and still process timers and Fds transparently
  procedure Pause (Timeout_Ms : in Integer);

  ----------------
  -- Exceptions --
  ----------------
  -- When setting a Cb on a fd that has already a Cb
  -- When deleting a Cb on a fd that has no Cb
  Fd_Cb_Error : exception;

  -- When Delay_Spec of Wait has a clock (i.e. is not real time)
  Invalid_Delay : exception;

  -- If a callback raises an exception
  Cb_Error : exception;

private

  -- For Handling child package

  -- Logger for debug
  package Logger is new Trace.Basic_Logger ("Event_Mng");

  -- Get kind of last signal
  type Signal_Kind_List is (Unknown_Sig, No_Sig, Dummy_Sig,
                            Terminate_Sig, Child_Sig, Usr1_Sig, Usr2_Sig);
  function Get_Signal_Kind return Signal_Kind_List;

  -- Registered callbacks
  function Get_Term_Cb return Sig_Callback;
  function Get_Child_Cb return Sig_Callback;
  function Get_Usr1_Cb return Sig_Callback;
  function Get_Usr2_Cb return Sig_Callback;

  -- Find the Cb of a read or write Fd
  type Cb_Rec is record
    Fd : File_Desc;
    Read : Boolean;
    Cb : Fd_Callback;
  end record;
  function Search_Cb (Cb : in out Cb_Rec) return Boolean;

end Event_Mng;

