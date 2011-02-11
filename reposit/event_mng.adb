with System, Ada.Text_Io, Ada.Io_Exceptions;
with C_Types, Null_Procedure, Dynamic_List, Environ, Timeval, Perpet,
     Virtual_Time;
package body Event_Mng is

  -------------
  -- General --
  -------------

  -- Result of a call to C
  Ok : constant C_Types.Int := 0;

  function For_Ada(C_Boolean : in C_Types.Bool) return Boolean is
  begin
    return Boolean'Val(C_Types.Bool'Pos(C_Boolean));
  end For_Ada;

  Debug_Var_Name : constant String := "EVENT_MNG_DEBUG";
  Debug : Boolean := False;
  Debug_Set : Boolean := False;

  procedure Set_Debug is
  begin
    if Debug_Set then
      return;
    end if;
    Debug := Environ.Is_Yes (Debug_Var_Name);
    Debug_Set := True;
  exception
    when others =>
      null;
  end Set_Debug;


  procedure Put_Debug (Msg : in String) is
  begin
    if Debug then
      Ada.Text_Io.Put_Line (Msg);
    end if;
  exception
    when Ada.Io_Exceptions.Device_Error =>
      null;
  end Put_Debug;

  ------------------------------------------------------------------

  -------------------
  -- Fd management --
  -------------------

  function C_Add_Fd (Fd : C_Types.Int; Read : C_Types.Bool) return C_Types.Int;
  pragma Import(C, C_Add_Fd, "evt_add_fd");

  function C_Del_Fd (Fd : C_Types.Int; Read : C_Types.Bool) return C_Types.Int;
  pragma Import(C, C_Del_Fd, "evt_del_fd");

  function C_Fd_Set (Fd : C_Types.Int; Read : C_Types.Bool) return C_Types.Bool;
  pragma Import(C, C_Fd_Set, "evt_fd_set");

  -- Callback list
  type Cb_Rec is record
    Fd : File_Desc;
    Read : Boolean;
    Cb : Fd_Callback;
  end record;
  package Cb_Dyn_Mng is new Dynamic_List(Cb_Rec);
  package Cb_Mng renames Cb_Dyn_Mng.Dyn_List;
  Cb_List : Cb_Mng.List_Type;

  -- Same Fd
  function Same_Fd (Cb1, Cb2 : Cb_Rec) return Boolean is
    use type Sys_Calls.File_Desc;
  begin
    return Cb1.Read = Cb2.Read and then Cb1.Fd = Cb2.Fd;
  end Same_Fd;
  procedure Cb_Search is new Cb_Mng.Search(Same_Fd);

  procedure Add_Fd_Callback (Fd : in File_Desc; Read : in Boolean;
                             Callback : in Fd_Callback) is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
    Found : Boolean;
  begin
    -- Check no cb for this fd yet
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Found, Cb_Searched, Cb_Mng.Prev,
               From => Cb_Mng.Absolute);
    if Found then
      raise Fd_Cb_Error;
    end if;

    -- Append
    Cb_List.Rewind (False, Cb_Mng.Prev);
    Cb_List.Insert ((Fd, Read, Callback));
    -- Add fd to select
    Res := C_Add_Fd (Integer(Fd), C_Types.Bool(Read)) = Ok;
    if not Res then
      raise Fd_Cb_Error;
    end if;
    Put_Debug ("Event_Mng.Add_Fd_Callback " & Fd'Img & " " & Read'Img);
  exception
    when others =>
      raise Fd_Cb_Error;
  end Add_Fd_Callback;

  procedure Del_Fd_Callback (Fd : in File_Desc; Read : in Boolean) is
    Res1, Res2 : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Del fd from select
    Res1 := C_Del_Fd (Integer(Fd), C_Types.Bool(Read)) = Ok;
    -- Del from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Res2, Cb_Searched, Cb_Mng.Prev,
               From => Cb_Mng.Absolute);
    if not Res2 then
      raise Fd_Cb_Error;
    end if;
    Cb_List.Delete (Moved => Res2);

    if not Res1 then
      raise Fd_Cb_Error;
    end if;

    Put_Debug ("Event_Mng.Del_Fd_Callback " & Fd'Img & " " & Read'Img);
  end Del_Fd_Callback;

  function Get_Fd_Callback (Fd : in File_Desc; Read : in Boolean)
           return Fd_Callback is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Get from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Res, Cb_Searched, Cb_Mng.Prev,
               From => Cb_Mng.Absolute);
    if not Res then
      return null;
    end if;
    Cb_List.Read (Cb_Searched, Cb_Mng.Current);
    return Cb_Searched.Cb;
  end Get_Fd_Callback;

  function Fd_Callback_Set (Fd : in File_Desc; Read : in Boolean)
  return Boolean is
    Res : C_Types.Bool;
  begin
    Res := C_Fd_Set (Integer(Fd), C_Types.Bool(Read));
    return Boolean(Res);
  end Fd_Callback_Set;

  ------------------------------------------------------------------

  -----------------------
  -- Signal management --
  -----------------------
  C_Sig_None      : constant Integer := -2;
  C_Sig_Unknown   : constant Integer := -1;
  C_Sig_Dummy     : constant Integer :=  0;
  C_Sig_Child     : constant Integer :=  1;
  C_Sig_Terminate : constant Integer :=  2;

  procedure C_Send_Signal (Num : C_Types.Int);
  pragma Import(C, C_Send_Signal, "send_signal");

  function C_Get_Signal return C_Types.Int;
  pragma Import(C, C_Get_Signal, "get_signal");

  procedure C_Activate_Signal_Handling;
  pragma Import(C, C_Activate_Signal_Handling, "activate_signal_handling");

  function C_Reset_Default_Signals return C_Types.Int;
  pragma Import(C, C_Reset_Default_Signals, "reset_default_signals");

  Cb_Term_Sig : Sig_Callback := Null_Procedure'Access;
  Cb_Child_Sig : Sig_Callback := Null_Procedure'Access;

  procedure Set_Sig_Term_Callback (Callback : in Sig_Callback) is
  begin
    if Callback /= null then
      Activate_Signal_Handling;
    end if;
    Cb_Term_Sig := Callback;
  end Set_Sig_Term_Callback;

  procedure Set_Sig_Child_Callback (Callback : in Sig_Callback) is
  begin
    if Callback /= null then
      Activate_Signal_Handling;
    end if;
    Cb_Child_Sig := Callback;
  end Set_Sig_Child_Callback;

  -- Return current Cb
  function Get_Sig_Term_Callback return Sig_Callback is
  begin
    return Cb_Term_Sig;
  end Get_Sig_Term_Callback;

  function Get_Sig_Child_Callback return Sig_Callback is
  begin
    return Cb_Child_Sig;
  end Get_Sig_Child_Callback;

  -- Is a callback set on signals
  function Sig_Term_Callback_Set return Boolean is
  begin
    return Cb_Term_Sig /= null;
  end Sig_Term_Callback_Set;

  function Sig_Child_Callback_Set return Boolean is
  begin
    return Cb_Child_Sig /= null;
  end Sig_Child_Callback_Set;

  procedure Send_Dummy_Signal is
  begin
    Put_Debug ("Event_Mng.Send_Signal");
    C_Send_Signal (C_Sig_Dummy);
  end Send_Dummy_Signal;

  procedure Activate_Signal_Handling is
  begin
    C_Activate_Signal_Handling;
  end Activate_Signal_Handling;

  function Reset_Default_Signals_Policy return Boolean is
    Sig : Integer;
  begin
    Sig := C_Reset_Default_Signals;
    return Sig = C_Sig_Terminate;
  end Reset_Default_Signals_Policy;
  procedure Reset_Default_Signals_Policy is
    Sigterm : constant := 15;
  begin
    if Reset_Default_Signals_Policy then
      Sys_Calls.Kill (Sys_Calls.Get_Pid, Sigterm);
    end if;
  end Reset_Default_Signals_Policy;

  -- PRIVATE. Get kind of last signal
  type Signal_Kind_List is (Unknown_Sig, No_Sig, Dummy_Sig,
                            Terminate_Sig, Child_Sig);
  function Get_Signal_Kind return Signal_Kind_List is
    Sig : Integer;
  begin
    Sig := C_Get_Signal;
    Put_Debug ( "Event_Mng.Get_Signal_Kind: C_Get_Signal => " & Sig'Img);
    case Sig is
      when C_Sig_Unknown =>
        return Unknown_Sig;
      when C_Sig_None =>
        return No_Sig;
      when C_Sig_Dummy =>
        return Dummy_Sig;
      when C_Sig_Terminate =>
        return Terminate_Sig;
      when C_Sig_Child =>
        return Child_Sig;
      when others =>
        return Unknown_Sig;
    end case;
  end Get_Signal_Kind;

  ------------------------------------------------------------------

  -------------------
  -- Waiting point --
  -------------------
  C_No_Event   : constant Integer := -1;
  C_Sig_Event  : constant Integer := -2;
  C_Wake_Event : constant Integer := -3;
  function C_Wait (P_Fd : System.Address;
                   P_Read : System.Address;
                   P_Timeout : System.Address) return C_Types.Int;
  pragma Import(C, C_Wait, "evt_wait");

  function Wait (Delay_Spec : Timers.Delay_Rec) return Out_Event_List is
    Fd    : Integer;
    Read  : C_Types.Bool;
    Final_Exp, Next_Exp : Timers.Expiration_Rec;
    Now : Virtual_Time.Time;
    Timeout : C_Types.Timeval_T;
    C_Res : C_Types.Int;
    Handle_Res : Out_Event_List;
    use type Virtual_Time.Clock_Access,
             Virtual_Time.Time, Virtual_Time.Speed_Range,
             Timers.Expiration_Rec, Perpet.Delta_Rec;
  begin
    Set_Debug;
    if Delay_Spec.Clock /= null then
      raise Invalid_Delay;
    end if;
    -- Compute final expiration in virtual time
    Now := Virtual_Time.Current_Time (Delay_Spec.Clock);
    case Delay_Spec.Delay_Kind is
      when Timers.Delay_Sec =>
        if Delay_Spec.Delay_Seconds < 0.0 then
          Final_Exp := (Infinite => True);
        else
          Final_Exp := (Infinite => False,
                        Time => Now + Delay_Spec.Delay_Seconds);
        end if;
      when Timers.Delay_Del =>
        Final_Exp := (Infinite => False,
                      Time => Now + Delay_Spec.Delay_Delta);
      when Timers.Delay_Exp =>
        Final_Exp := (Infinite => False,
                      Time => Delay_Spec.Expiration_Time);
    end case;

    loop

      -- Compute next timeout
      Next_Exp := Timers.Next_Expiration (Final_Exp);
      if Next_Exp = Timers.Infinite_Expiration then
        Timeout := Timeval.Infinite_C_Timeout;
      else
        Now := Virtual_Time.Current_Time (Delay_Spec.Clock);
        if Now < Next_Exp.Time then
          Timeout := Timeval.To_C_Timeout (Next_Exp.Time - Now);
        else
          Timeout := (0, 0);
        end if;
      end if;

      -- Wait
      Put_Debug ("Event_Mng.Wait timeout " & Timeval.Image(Timeout));
      C_Res := C_Wait (Fd'Address, Read'Address, Timeout'Address);
      if C_Res /= Ok then
        Put_Debug ("Event_Mng.Wait.C_Wait -> ERROR");
        return No_Event;
      else
        Put_Debug ("Event_Mng.Wait.C_Wait -> "
                 & Integer'Image(Fd)
                 & " " & C_Types.Bool'Image(Read));
      end if;

      -- Results
      if Fd = C_Sig_Event then
        -- Signal
        Handle_Res := Handle ((Kind => Signal_Event));
      elsif Fd = C_Wake_Event then
        -- Wake_up event => skip
        Handle_Res := No_Event;
      elsif Fd = C_No_Event or else Fd >= 0 then
        -- Expire timers?
        Handle_Res := Handle ((Kind => No_Event));
        if Handle_Res = No_Event and then Fd >= 0 then
          -- No timer event to report and a fd set: handle fd
          Handle_Res := Handle ((Kind => Fd_Event,
                          Fd => File_Desc(Fd),
                          Read => For_Ada(Read)));
        end if;
      else
        Handle_Res := No_Event;
        Put_Debug ("Event_Mng.Wait Invalid fd");
      end if;
      Put_Debug ("Event_Mng.Wait Handle -> " & Handle_Res'Img);

      -- Done on event or timeout
      if Handle_Res /= No_Event then
        return Handle_Res;
      end if;
      if Timers.Is_Reached (Final_Exp) then
        -- Requested timeout reached
        return No_Event;
      end if;

    end loop;

  end Wait;

  function Wait (Timeout_Ms : Integer) return Out_Event_List is
    Dur : Duration;
  begin
    if Timeout_Ms >= 0 then
      Dur := Duration (Timeout_Ms) / 1_000.0;
    else
      Dur := Timers.Infinite_Seconds;
    end if;
    return Wait ((Delay_Kind    => Timers.Delay_Sec,
                  Clock         => null,
                  Period        => Timers.No_Period,
                  Delay_Seconds => Dur));
  end Wait;

  function Wait (Timeout_Ms : Integer) return Boolean is
    Event : Out_Event_List;
  begin
    Event := Wait (Timeout_Ms);
    return Event /= No_Event;
  end Wait;

  procedure Wait (Timeout_Ms : Integer) is
    Event : Out_Event_List;
    pragma Unreferenced (Event);
  begin
    Event := Wait (Timeout_Ms);
  end Wait;

  -- The pause
  Pause_Level : Natural := 0;

  function Pause_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data;
                     New_Id : Timers.Timer_Id := Timers.No_Timer)
           return Boolean is
    pragma Unreferenced (Id, New_Id);
  begin
    -- Check this expiration versus current pause level
    if Pause_Level >= Data then
      -- Pop up to current level
      Pause_Level := Data - 1;
    end if;
    Put_Debug ("Event_Mng.Pause.Cb " & Data'Img);
    return True;
  end Pause_Cb;

  procedure Pause (Timeout_Ms : in Integer) is
    Tid : Timers.Timer_Id;
    Loc_Level : Positive;
    Wait_Timeout : Integer;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Set_Debug;

    -- Pause is released by a Dummy signal
    Activate_Signal_Handling;

    -- Increment global pause level and store ours
    Pause_Level := Pause_Level + 1;
    Loc_Level := Pause_Level;
    Put_Debug ("Event_Mng.Pause Push " & Loc_Level'Img);

    -- Arm or simulate timer
    if Timeout_Ms < 0 then
      Wait_Timeout := Infinite_Ms;
      Put_Debug ("Event_Mng.Pause.Infinite");
    elsif Timeout_Ms = 0 then
      Dummy := Pause_Cb (Timers.No_Timer, Pause_Level);
      Wait_Timeout := 0;
    else
      -- Arm a timer
      Tid.Create ( (Delay_Kind    => Timers.Delay_Sec,
                    Clock         => null,
                    Period        => Timers.No_Period,
                    Delay_Seconds => Duration(Timeout_Ms)/1000.0),
                      Callback => Pause_Cb'Access,
                      Data     => Pause_Level);
      Wait_Timeout := Infinite_Ms;
    end if;

    -- Wait for this or a lower level pause expiration
    -- or signal
    loop

      if Wait (Wait_Timeout) = Signal_Event then
        -- Exit all pauses on signal
        Put_Debug ("Event_Mng.Pause Signal " & Pause_Level'Img);
        if Pause_Level /= 0 then
          Pause_Level := Pause_Level - 1;
          if Pause_Level /= 0 then
            Send_Dummy_Signal;
          end if;
        end if;
        -- Cancel timer if not yet expired
        Tid.Delete_If_Exists;
        exit;
      end if;

      exit when Pause_Level < Loc_Level;
    end loop;
  end Pause;

  ------------------------------------------------------------------
  --------------------------
  -- Low level operations --
  --------------------------
  function Handle (Event : Event_Rec) return Out_Event_List is
    Cb_Searched : Cb_Rec;
    Signal_Kind : Signal_Kind_List;
    Cb_Found : Boolean;
  begin
    Set_Debug;
    Put_Debug ("Event_Mng.Handle event " & Event.Kind'Img);
    case Event.Kind is
      when Fd_Event =>
        -- A FD event
        Cb_Searched.Fd := Event.Fd;
        Cb_Searched.Read := Event.Read;
        Cb_Searched.Cb := null;
        -- Search and read callback
        Cb_Search (Cb_List, Cb_Found, Cb_Searched, From => Cb_Mng.Absolute);
        if not Cb_Found then
          Put_Debug ("**** Event_Mng.Handle: "
                   & File_Desc'Image(Event.Fd)
                   & " fd not found ****");
        else
          Cb_List.Read (Cb_Searched,  Cb_Mng.Current);
          Put_Debug ("Event_Mng.Handle calling Cb on fd "
                   & Event.Fd'Img & " " & Event.Read'Img);
          -- Call it and propagate event if callback returns true
          if Cb_Searched.Cb /= null then
            if Cb_Searched.Cb (Cb_Searched.Fd, Cb_Searched.Read) then
              return Fd_Event;
            end if;
          end if;
        end if;
      when Signal_Event =>
        Signal_Kind := Get_Signal_Kind;
        Put_Debug ("Event_Mng.Handle " & Signal_Kind'Img
                 & " with term cb: " & Boolean'Image(Cb_Term_Sig /= null)
                 & " and child cb: " & Boolean'Image(Cb_Child_Sig /= null));
        case Signal_Kind is
          when Unknown_Sig | No_Sig =>
            -- No_Event
            null;
          when Dummy_Sig =>
            -- Dummy signal: never call Cb but always generate event
            return Signal_Event;
          when Terminate_Sig =>
            if Cb_Term_Sig /= null then
              Cb_Term_Sig.all;
              return Signal_Event;
            end if;
            -- else No_Event
          when Child_Sig =>
            if Cb_Child_Sig /= null then
              Cb_Child_Sig.all;
              return Signal_Event;
            end if;
            -- else No_Event
        end case;
      when No_Event =>
        -- Nothing. Expire timers or return timeout
        if Timers.Expire then
          Put_Debug ("Event_Mng.Handle: No_Event -> Timer_Event");
          return Timer_Event;
        end if;
    end case;
    return No_Event;

  end Handle;

end Event_Mng;

