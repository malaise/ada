with System, Ada.Calendar, Ada.Text_Io;
with Null_Procedure, Dynamic_List, Timers, Environ;
package body Event_Mng is

  -------------
  -- General --
  -------------

  -- Result of a call to C
  subtype Result is Integer;
  Ok : constant Result := 0;

  -- Boolean on 32 bits for C
  type Bool_For_C is new Boolean;
  for Bool_For_C'Size use 32;
  for Bool_For_C use (False => 0, True => 1);

  function For_C(Ada_Boolean : in Boolean) return Bool_For_C is
  begin
    return Bool_For_C'Val(Boolean'Pos(Ada_Boolean));
  end For_C;

  function For_Ada(C_Boolean : in Bool_For_C) return Boolean is
  begin
    return Boolean'Val(Bool_For_C'Pos(C_Boolean));
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


  ------------------------------------------------------------------

  -------------------
  -- Fd management --
  -------------------

  function C_Add_Fd (Fd : Integer; Read : Bool_For_C) return Result;
  pragma Import(C, C_Add_Fd, "evt_add_fd");

  function C_Del_Fd (Fd : Integer; Read : Bool_For_C) return Result;
  pragma Import(C, C_Del_Fd, "evt_del_fd");

  function C_Fd_Set (Fd : Integer; Read : Bool_For_C) return Bool_For_C;
  pragma Import(C, C_Fd_Set, "evt_fd_set");

  -- Callback list
  type Cb_Rec is record
    Fd : File_Desc;
    Read : Boolean;
    Cb : Fd_Callback;
  end record;
  package Cb_Mng is new Dynamic_List(Cb_Rec);
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
    Cb_Search (Cb_List, Found, Cb_Searched, Cb_Mng.Prev, From => Cb_Mng.Absolute);
    if Found then
      raise Event_Failure;
    end if;

    -- Append
    if not Cb_Mng.Is_Empty (Cb_List) then
      Cb_Mng.Move_To (Cb_List, Cb_Mng.Prev, 0, False);
    end if;
    Cb_Mng.Insert (Cb_List, (Fd, Read, Callback));
    -- Add fd to select
    Res := C_Add_Fd (Integer(Fd), Bool_For_C(Read)) = Ok;
    if not Res then
      raise Event_Failure;
    end if;
    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Add_Fd_Callback "
                          & Fd'Img & " " & Read'Img);
    end if;
  exception
    when others =>
      raise Event_Failure;
  end Add_Fd_Callback;

  procedure Del_Fd_Callback (Fd : in File_Desc; Read : in Boolean) is
    Res1, Res2 : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Del fd from select
    Res1 := C_Del_Fd (Integer(Fd), Bool_For_C(Read)) = Ok;
    -- Del from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Res2, Cb_Searched, Cb_Mng.Prev, From => Cb_Mng.Absolute);
    if not Res2 then
      raise Event_Failure;
    end if;
    Cb_Mng.Delete (Cb_List, Done => Res2);

    if not Res1 then
      raise Event_Failure;
    end if;

    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Del_Fd_Callback "
                          & Fd'Img & " " & Read'Img);
    end if;
  end Del_Fd_Callback;

  function Fd_Callback_Set (Fd : in File_Desc; Read : in Boolean)
  return Boolean is
    Res : Bool_For_C;
  begin
    Res := C_Fd_Set (Integer(Fd), Bool_For_C(Read));
    return Boolean(Res);
  end Fd_Callback_Set;


  ------------------------------------------------------------------

  -----------------------
  -- Signal management --
  -----------------------
  C_Sig_Unknown   : constant Integer := -2;
  C_Sig_None      : constant Integer := -1;
  C_Sig_Dummy     : constant Integer :=  0;
  C_Sig_Terminate : constant Integer :=  1;
  C_Sig_Child     : constant Integer :=  2;

  procedure C_Send_Signal (Num : Integer);
  pragma Import(C, C_Send_Signal, "send_signal");

  function C_Get_Signal return Integer;
  pragma Import(C, C_Get_Signal, "get_signal");

  Cb_Term_Sig : Sig_Callback := Null_Procedure'Access;
  Cb_Child_Sig : Sig_Callback := Null_Procedure'Access;

  procedure Set_Sig_Term_Callback (Callback : in Sig_Callback) is
  begin
    Cb_Term_Sig := Callback;
  end Set_Sig_Term_Callback;

  procedure Set_Sig_Child_Callback (Callback : in Sig_Callback) is
  begin
    Cb_Child_Sig := Callback;
  end Set_Sig_Child_Callback;

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
    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Send_Signal");
    end if;
    C_Send_Signal (C_Sig_Dummy);
  end Send_Dummy_Signal;
  
  -- PRIVATE. Get kind of last signal
  type Signal_Kind_List is (Unknown_Sig, No_Sig, Dummy_Sig,
                            Terminate_Sig, Child_Sig);
  function Get_Signal_Kind return Signal_Kind_List is
    Sig : Integer;
  begin
    Sig := C_Get_Signal;
    if Debug then
      Ada.Text_Io.Put_Line (
       "Event_Mng.Get_Signal_Kind: C_Get_Signal => " & Sig'Img);
    end if;
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
  C_No_Event  : constant Integer := -1;
  C_Sig_Event : constant Integer := -2;
  function C_Wait (P_Fd : System.Address;
                   P_Read : System.Address;
                   Timeout_Ms : System.Address) return Result;
  pragma Import(C, C_Wait, "evt_wait");

  procedure C_Wake_Up;
  pragma Import(C, C_Wake_Up, "evt_wake_up");

  Infinite_Timeout : constant Duration := Timers.Infinite_Seconds;

  function Wait (Timeout_Ms : Integer) return Out_Event_List is
    Fd    : Integer;
    Read  : Bool_For_C;
    Final_Exp : Timers.Expiration_Rec;
    Timeout_In, Timeout_Dur : Duration;
    Timeout_Wait : Integer;
    C_Res : Result;
    Handle_Res : Out_Event_List;
    use type  Ada.Calendar.Time;
  begin
    Set_Debug;
    -- Compute final expiration
    if Timeout_Ms >= 0 then
      Timeout_In := Duration (Timeout_Ms) / 1000.0;
      Final_Exp := (Infinite => False,
                    Time => Ada.Calendar.Clock + Timeout_In);
    else
      Timeout_In := Infinite_Timeout;
      Final_Exp := (Infinite => True);
    end if;

    loop
      -- Compute next timeout
      Timeout_Dur := Timers.Next_Timeout (Final_Exp);

      -- Wait
      Timeout_Wait := Integer (Timeout_Dur * 1000.0);
      if Debug then
        Ada.Text_Io.Put_Line ("Event_Mng.Wait timeout " & Timeout_Wait'Img);
      end if;
      C_Res := C_Wait (Fd'Address, Read'Address, Timeout_Wait'Address);
      if Debug then
        if C_Res /= Ok then
          Ada.Text_Io.Put_Line ("Event_Mng.Wait.C_Wait -> ERROR");
          return No_Event;
        else
          Ada.Text_Io.Put_Line ("Event_Mng.Wait.C_Wait -> "
                                         & Integer'Image(Fd)
                                         & " " & Bool_For_C'Image(Read));
        end if;
      end if;

      -- Results
      if Fd = C_Sig_Event then
        -- Signal
        Handle_Res := Handle ((Kind => Sig_Event));
      elsif Fd = C_No_Event or else Fd >= 0 then
        -- Expire timers?
        Handle_Res := Handle ((Kind => No_Event));
        if Handle_Res = No_Event and then Fd >= 0 then 
        -- No timer and fd: fd
          Handle_Res := Handle ((Kind => Fd_Event,
                          Fd => File_Desc(Fd),
                          Read => For_Ada(Read)));
        end if;
      else
        Handle_Res := No_Event;
        if Debug then
          Ada.Text_Io.Put_Line ("Event_Mng.Wait Invalid fd");
        end if;
      end if;
      if Debug then
        Ada.Text_Io.Put_Line ("Event_Mng.Wait Handle -> " & Handle_Res'Img);
      end if;

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

  function Wait (Timeout_Ms : Integer) return Boolean is
    Event : Out_Event_List;
  begin
    Event := Wait (Timeout_Ms);
    return Event /= No_Event;
  end Wait;

  procedure Wait (Timeout_Ms : Integer) is
    Event : Out_Event_List;
  begin
    Event := Wait (Timeout_Ms);
  end Wait;

  -- The pause
  Pause_Level : Natural := 0;

  function Pause_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
  begin
    -- Check this expiration versus current pause level
    if Pause_Level >= Data then
      -- Pop up to current level
      Pause_Level := Data - 1;
    end if;
    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Pause.Cb " & Data'Img);
    end if;
    return True;
  end Pause_Cb;

  procedure Pause (Timeout_Ms : in Integer) is
     Tid : Timers.Timer_Id := Timers.No_Timer;
     Loc_Level : Positive;
     Wait_Timeout : Integer;
     Dummy : Boolean;
  begin
    Set_Debug;

    -- Increment global pause level and store ours
    Pause_Level := Pause_Level + 1;
    Loc_Level := Pause_Level;
    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Pause Push " & Loc_Level'Img);
    end if;

    -- Arm or simulate timer
    if Timeout_Ms < 0 then
      Wait_Timeout := Infinite_Ms;
      if Debug then
        Ada.Text_Io.Put_Line ("Event_Mng.Pause.Infinite");
      end if;
    elsif Timeout_Ms = 0 then
      Dummy := Pause_Cb (Timers.No_Timer, Pause_Level);
      Wait_Timeout := 0;
    else
      -- Arm a timer
      Tid := Timers.Create ( (Timers.Delay_Sec,
                              Timers.No_Period,
                              Duration(Timeout_Ms)/1000.0),
                             Pause_Cb'Access,
                             Pause_Level);
      Wait_Timeout := Infinite_Ms;
    end if;

    -- Wait for this or a lower level pause expiration
    -- or signal
    loop

      if Wait (Wait_Timeout) = Sig_Event then
        -- Exit all pauses on signal
        if Debug then
          Ada.Text_Io.Put_Line ("Event_Mng.Pause Signal " & Pause_Level'Img);
        end if;
        if Pause_Level /= 0 then
          Pause_Level := Pause_Level - 1;
          if Pause_Level /= 0 then
            Send_Dummy_Signal;
          end if;
        end if;
        exit;
      end if;

      exit when Pause_Level < Loc_Level;
    end loop;
  end Pause;

  --

  procedure Wake_Up is
  begin
    C_Wake_Up;
  end Wake_Up;

  function Handle (Event : Event_Rec) return Out_Event_List is
    Cb_Searched : Cb_Rec;
    Signal_Kind : Signal_Kind_List;
    Cb_Found : Boolean;
  begin
    Set_Debug;
    if Debug then
      Ada.Text_Io.Put_Line ("Event_Mng.Handle event " & Event.Kind'Img);
    end if;
    case Event.Kind is
      when Fd_Event =>
        -- A FD event
        Cb_Searched.Fd := Event.Fd;
        Cb_Searched.Read := Event.Read;
        Cb_Searched.Cb := null;
        -- Search and read callback
        Cb_Search (Cb_List, Cb_Found, Cb_Searched, From => Cb_Mng.Absolute);
        if not Cb_Found then
          if Debug then
            Ada.Text_Io.Put_Line ("**** Event_Mng.Handle: "
                                & File_Desc'Image(Event.Fd)
                                & " fd not found ****");
          end if;
        else
          Cb_Mng.Read (Cb_List, Cb_Searched,  Cb_Mng.Current);
          if Debug then
            Ada.Text_Io.Put_Line ("Event_Mng.Handle calling Cb on fd "
                   & Event.Fd'Img & " " & Event.Read'Img);
          end if;
          -- Call it and propagate event if callback returns true
          if Cb_Searched.Cb /= null then
            if Cb_Searched.Cb (Cb_Searched.Fd, Cb_Searched.Read) then
              return Fd_Event;
            end if;
          end if;
        end if;
      when Sig_Event =>
        Signal_Kind := Get_Signal_Kind;
        if Debug then
          Ada.Text_Io.Put_Line ("Event_Mng.Handle " & Signal_Kind'Img
                   & " with term cb: " & Boolean'Image(Cb_Term_Sig /= null)
                   & " and child cb: " & Boolean'Image(Cb_Child_Sig /= null));
        end if;
        case Signal_Kind is
          when Unknown_Sig | No_Sig =>
            -- No_Event
            null;
          when Dummy_Sig =>
            -- Dummy signal: never call Cb but always generate event
            return Sig_Event;
          when Terminate_Sig =>
            if Cb_Term_Sig /= null then
              Cb_Term_Sig.all;
              return Sig_Event;
            end if;
            -- else No_Event
          when Child_Sig =>
            if Cb_Child_Sig /= null then
              Cb_Child_Sig.all;
              return Sig_Event;
            end if;
            -- else No_Event
        end case;
      when No_Event =>
        -- Nothing. Expire timers or return timeout
        if Timers.Expire then
          return Timer_Event;
        end if;
    end case;
    return No_Event;

  end Handle;

end Event_Mng;

