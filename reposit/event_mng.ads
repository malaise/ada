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
  -- Called on Sig Int
  type Sig_Callback is access procedure;
  -- Register a callback on termination signal (SigInt)
  -- Call it with null to disable generation of Signal_Event by Wait
  -- Default is event generation with no callback
  procedure Set_Sig_Callback (Callback : in Sig_Callback);
  -- Is a callback set on signals
  function Sig_Callback_Set return Boolean;


  -------------------
  -- Waiting point --
  -------------------
  -- Wait until some event callback return True, or until timeout
  type Out_Event_List is (Timer_Event, Fd_Event, Sig_Event, No_Event);
  function Wait (Timeout_Ms : Integer) return Out_Event_List;
  function Wait (Timeout_Ms : Integer) return Boolean;
  Procedure Wait (Timeout_Ms : Integer);

  -- Force re-evaluation (and expiration) of timers while in Wait
  procedure Wake_Up;


  ----------------------
  -- Event management --
  ----------------------
  -- Event got by another waiting point (X_Select?)
  subtype In_Event_List is Out_Event_List range Fd_Event .. No_Event;
  type Event_Rec (Kind : In_Event_List := Fd_Event) is record
    case Kind is
      when Fd_Event =>
        Fd : File_Desc;
        Read : Boolean;
      when Sig_Event | No_Event =>
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

