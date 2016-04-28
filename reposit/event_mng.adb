with System;
with C_Types, Null_Procedure, Dynamic_List, Timeval, Perpet,
     Any_Def, My_Math, Virtual_Time, Timers.Expiration;
with Event_Mng.Handling;
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
  package Cb_Dyn_Mng is new Dynamic_List(Cb_Rec);
  package Cb_Mng renames Cb_Dyn_Mng.Dyn_List;
  Cb_List : Cb_Mng.List_Type;

  -- Same Fd
  function Same_Fd (Cb1, Cb2 : Cb_Rec) return Boolean is
    use type Sys_Calls.File_Desc;
  begin
    return Cb1.Read = Cb2.Read and then Cb1.Fd = Cb2.Fd;
  end Same_Fd;
  function Cb_Search is new Cb_Mng.Search(Same_Fd);

  -- Find the Cb of a read or write Fd
  function Search_Cb (Cb : in out Cb_Rec) return Boolean is

  begin
    -- Complete criteria
    Cb.Cb := null;
    -- Search
    if not Cb_Search (Cb_List, Cb, From => Cb_Mng.Absolute) then
      return False;
    else
      -- Read
      Cb_List.Read (Cb,  Cb_Mng.Current);
      return True;
    end if;
  end Search_Cb;

  procedure Add_Fd_Callback (Fd : in File_Desc; Read : in Boolean;
                             Callback : in Fd_Callback) is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Check no cb for this fd yet
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    if Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev,
                  From => Cb_Mng.Current_Absolute) then
      raise Fd_Cb_Error;
    end if;

    -- Append
    Cb_List.Rewind (Cb_Mng.Prev, False);
    Cb_List.Insert ((Fd, Read, Callback));
    -- Add fd to select
    Res := C_Add_Fd (Integer(Fd), C_Types.Bool(Read)) = Ok;
    if not Res then
      raise Fd_Cb_Error;
    end if;
    Logger.Log_Debug ("Event_Mng.Add_Fd_Callback " & Fd'Img & " " & Read'Img);
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
    if not Cb_Search (Cb_List,  Cb_Searched, Cb_Mng.Prev,
                      From => Cb_Mng.Current_Absolute) then
      raise Fd_Cb_Error;
    end if;
    Cb_List.Delete (Moved => Res2);

    if not Res1 then
      raise Fd_Cb_Error;
    end if;

    Logger.Log_Debug ("Event_Mng.Del_Fd_Callback " & Fd'Img & " " & Read'Img);
  end Del_Fd_Callback;

  function Get_Fd_Callback (Fd : in File_Desc; Read : in Boolean)
           return Fd_Callback is
    Cb_Searched : Cb_Rec;
  begin
    -- Get from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    if not Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev,
                      From => Cb_Mng.Absolute) then
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

  procedure C_Send_Dummy_Signal;
  pragma Import(C, C_Send_Dummy_Signal, "send_dummy_signal");

  function C_Get_Signal return C_Types.Int;
  pragma Import(C, C_Get_Signal, "get_signal");

  function C_Reset_Default_Signals return C_Types.Int;
  pragma Import(C, C_Reset_Default_Signals, "reset_default_signals");

  function C_Signal_Handling_Set return C_Types.Bool;
  pragma Import(C, C_Signal_Handling_Set, "signal_handling_set");

  Cb_Term_Sig : Sig_Callback := Null_Procedure'Access;
  Cb_Child_Sig : Sig_Callback := Null_Procedure'Access;

  function Get_Term_Cb return Sig_Callback is
  begin
    return Cb_Term_Sig;
  end Get_Term_Cb;
  function Get_Child_Cb return Sig_Callback is
  begin
    return Cb_Child_Sig;
  end Get_Child_Cb;

  procedure Set_Sig_Term_Callback (Callback : in Sig_Callback) is
  begin
    Cb_Term_Sig := Callback;
  end Set_Sig_Term_Callback;

  procedure Set_Sig_Child_Callback (Callback : in Sig_Callback) is
  begin
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
    Logger.Log_Debug ("Event_Mng.Send_Dummy_Signal");
    C_Send_Dummy_Signal;
  end Send_Dummy_Signal;

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

  function Are_Signals_Handled return Boolean is
  begin
    return Boolean(C_Signal_Handling_Set);
  end Are_Signals_Handled;

  -- PRIVATE. Get kind of last signal
  function Get_Signal_Kind return Signal_Kind_List is
    Sig : Integer;
  begin
    Sig := C_Get_Signal;
    Logger.Log_Debug ( "Event_Mng.Get_Signal_Kind: C_Get_Signal => " & Sig'Img);
    return (case Sig is
              when C_Sig_Unknown =>   Unknown_Sig,
              when C_Sig_None =>      No_Sig,
              when C_Sig_Dummy =>     Dummy_Sig,
              when C_Sig_Terminate => Terminate_Sig,
              when C_Sig_Child =>     Child_Sig,
              when others =>          Unknown_Sig);
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
    Final_Exp, Next_Exp : Timers.Expiration.Expiration_Rec;
    Now : Virtual_Time.Time;
    Timeout_Val : C_Types.Timeval_T;
    C_Res : C_Types.Int;
    Handle_Res : Out_Event_List;
    use type Virtual_Time.Clock_Access,
             Virtual_Time.Time, Virtual_Time.Speed_Range,
             Timers.Expiration.Expiration_Rec, Perpet.Delta_Rec;
  begin
    if Delay_Spec.Clock /= null then
      raise Invalid_Delay;
    end if;
    -- Compute final expiration in virtual time
    Now := Virtual_Time.Current_Time (Delay_Spec.Clock);
    Final_Exp := (case Delay_Spec.Delay_Kind is
        when Timers.Delay_Sec =>
          (if Delay_Spec.Delay_Seconds < 0.0 then (Infinite => True)
           else (Infinite => False, Time => Now + Delay_Spec.Delay_Seconds)),
        when Timers.Delay_Del => (Infinite => False,
                                  Time => Now + Delay_Spec.Delay_Delta),
        when Timers.Delay_Exp => (Infinite => False,
                                  Time => Delay_Spec.Expiration_Time));

    loop

      -- Compute next timeout
      Next_Exp := Timers.Expiration.Next_Expiration (Final_Exp);
      if Next_Exp = Timers.Expiration.Infinite_Expiration then
        Timeout_Val := Timeval.Infinite_Timeout;
      else
        Now := Virtual_Time.Current_Time (Delay_Spec.Clock);
        Timeout_Val := (if Now < Next_Exp.Time then
                          Timeval.Delta2Timeout (Next_Exp.Time - Now)
                        else
                          (0, 0));
      end if;

      -- Wait
      Logger.Log_Debug ("Event_Mng.Wait timeout " & Timeval.Image(Timeout_Val));
      C_Res := C_Wait (Fd'Address, Read'Address, Timeout_Val'Address);
      if C_Res /= Ok then
        Logger.Log_Debug ("Event_Mng.Wait.C_Wait -> ERROR");
        return Timeout;
      end if;
      Logger.Log_Debug ("Event_Mng.Wait.C_Wait -> "
               & Integer'Image(Fd)
               & " " & C_Types.Bool'Image(Read));

      -- Results
      if Fd = C_Sig_Event then
        -- Signal
        Handle_Res := Event_Mng.Handling.Handle ((Kind => Signal_Event));
      elsif Fd = C_Wake_Event then
        -- Wake_up event => skip
        Handle_Res := Timeout;
      elsif Fd = C_No_Event or else Fd >= 0 then
        -- Expire timers?
        Handle_Res := Event_Mng.Handling.Handle ((Kind => Timeout));
        if Handle_Res = Timeout and then Fd >= 0 then
          -- No timer event to report and a fd set: handle fd
          Handle_Res := Event_Mng.Handling.Handle (
                           (Kind => Fd_Event,
                            Fd => File_Desc(Fd),
                            Read => For_Ada(Read)));
        end if;
      else
        Handle_Res := Timeout;
        Logger.Log_Debug ("Event_Mng.Wait Invalid fd");
      end if;
      Logger.Log_Debug ("Event_Mng.Wait Handle -> " & Handle_Res'Img);

      -- Done on event or timeout
      if Handle_Res /= Timeout then
        return Handle_Res;
      end if;
      if Timers.Expiration.Is_Reached (Final_Exp) then
        -- Requested timeout reached
        return Timeout;
      end if;

    end loop;

  end Wait;

  function Wait (Timeout_Ms : Integer) return Out_Event_List is
    Dur : Duration;
  begin
    Dur := (if Timeout_Ms >= 0 then Duration (Timeout_Ms) / 1_000.0
            else Timers.Infinite_Seconds);
    return Wait ((Delay_Kind    => Timers.Delay_Sec,
                  Clock         => null,
                  Period        => Timers.No_Period,
                  Delay_Seconds => Dur));
  end Wait;

  function Wait (Timeout_Ms : Integer) return Boolean is
    Event : Out_Event_List;
  begin
    Event := Wait (Timeout_Ms);
    return Event /= Timeout;
  end Wait;

  procedure Wait (Timeout_Ms : Integer) is
    Dummy_Event : Out_Event_List;
  begin
    Dummy_Event := Wait (Timeout_Ms);
  end Wait;

  -- The pause
  Pause_Level : Any_Def.Any (Any_Def.Inte_Kind) := (Any_Def.Inte_Kind, 0);

  function Pause_Cb (Unused_Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data)
           return Boolean is
  begin
    -- Check this expiration versus current pause level
    if Pause_Level.Inte >= Data.Inte then
      -- Pop up to current level
      Pause_Level.Inte := Data.Inte - 1;
    end if;
    Logger.Log_Debug ("Event_Mng.Pause.Cb " & Any_Def.Image (Data));
    return True;
  end Pause_Cb;

  procedure Pause (Timeout_Ms : in Integer) is
    Tid : Timers.Timer_Id;
    Loc_Level : Any_Def.Any (Any_Def.Inte_Kind);
    Wait_Timeout : Integer;
    Dummy : Boolean;
    use type My_Math.Inte;
  begin
    -- Increment global pause level and store ours
    Pause_Level.Inte := Pause_Level.Inte + 1;
    Loc_Level := Pause_Level;
    Logger.Log_Debug ("Event_Mng.Pause Push " & Any_Def.Image (Loc_Level));

    -- Arm or simulate timer
    if Timeout_Ms < 0 then
      Wait_Timeout := Infinite_Ms;
      Logger.Log_Debug ("Event_Mng.Pause.Infinite");
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
    --  or a signal
    loop
      exit when Wait (Wait_Timeout) = Signal_Event
      or else Pause_Level.Inte < Loc_Level.Inte;
    end loop;
  end Pause;

end Event_Mng;

