with System, Ada.Calendar, Ada.Text_Io;
with Null_Procedure, Dynamic_List, Timers;
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
    Set : Boolean;
    Tru : Boolean;
    Val : String (1 .. 1);
    Len : Natural;
  begin
    if Debug_Set then
      return;
    end if;
    Sys_Calls.Getenv (Debug_Var_Name, Set, Tru, Val, Len);
    if Set and then (Val(1) = 'y' or else Val(1) = 'Y') then
      Debug := True;
    end if;
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

  -- Same FD
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
  begin
    -- Check no cb for this fd yet
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    begin
      Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev, From_Current => False);
      raise Event_Failure;
    exception
      when Cb_Mng.Not_In_List =>
        null;
    end;
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
  exception
    when others =>
      raise Event_Failure;
  end Add_Fd_Callback;

  procedure Del_Fd_Callback (Fd : in File_Desc; Read : in Boolean) is
    Res : Boolean;
    Cb_Searched : Cb_Rec;
  begin
    -- Del fd from select
    Res := C_Del_Fd (Integer(Fd), Bool_For_C(Read)) = Ok;
    -- del from list
    Cb_Searched.Fd := Fd;
    Cb_Searched.Read := Read;
    Cb_Searched.Cb := null;
    Cb_Search (Cb_List, Cb_Searched, Cb_Mng.Prev, From_Current => False);
    if Cb_Mng.Get_Position (Cb_List) /=  Cb_Mng.List_Length(Cb_List) then
      Cb_Mng.Delete (Cb_List, Cb_Mng.Next);
    else
      Cb_Mng.Delete (Cb_List, Cb_Mng.Prev);
    end if;
    if not Res then
      raise Event_Failure;
    end if;
  exception
    when others =>
      raise Event_Failure;
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
  Cb_Sig : Sig_Callback := Null_Procedure'Access;
  
  procedure Set_Sig_Callback (Callback : in Sig_Callback) is
  begin
    Cb_Sig := Callback;
  end Set_Sig_Callback;

  -- Is a callback set on signals
  function Sig_Callback_Set return Boolean is
  begin
    return Cb_Sig /= null;
  end Sig_Callback_Set;

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
      Final_Exp := (Infinite => False, Time => Ada.Calendar.Clock + Timeout_In);
    else
      Timeout_In := Infinite_Timeout;
      Final_Exp := (Infinite => True);
    end if;

    loop
      -- Compute next timeout
      Timeout_Dur := Timers.Next_Timeout (Final_Exp);

      -- Wait
      Timeout_Wait := Integer (Timeout_Dur * 1000.0);
      C_Res := C_Wait (Fd'Address, Read'Address, Timeout_Wait'Address);
      if Debug then
        if C_Res /= Ok then
          Ada.Text_Io.Put_Line ("  C_Wait -> ERROR");
          return No_Event;
        else
          Ada.Text_Io.Put_Line ("  C_Wait -> " & Integer'Image(Fd)
                                         & " " & Bool_For_C'Image(Read));
        end if;
      end if;

      -- Results
      if Fd = C_Sig_Event then
        -- Signal
        Handle_Res := Handle ((Kind => Sig_Event));
      elsif Fd >= 0 then
        Handle_Res := Handle ((Kind => Fd_Event,
                        Fd => File_Desc(Fd),
                        Read => For_Ada(Read)));
      elsif Fd = C_No_Event then
        -- Nothing. Expire timers
        Handle_Res := Handle ((Kind => No_Event));
      else
        Handle_Res := No_Event;
        if Debug then
          Ada.Text_Io.Put_Line ("  Wait -> Invalid fd");
        end if;
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


  procedure Wake_Up is
  begin
    C_Wake_Up;
  end Wake_Up;

  function Handle (Event : Event_Rec) return Out_Event_List is
    Cb_Searched : Cb_Rec;
  begin      
    Set_Debug;
    case Event.Kind is
      when Fd_Event =>
        -- A FD event
        Cb_Searched.Fd := Event.Fd;
        Cb_Searched.Read := Event.Read;
        Cb_Searched.Cb := null;
        begin
          -- Search and read callback
          Cb_Search (Cb_List, Cb_Searched, From_Current => False);
          Cb_Mng.Read (Cb_List, Cb_Searched,  Cb_Mng.Current);
          -- Call it and propagate event if callback returns true
          if Cb_Searched.Cb /= null then
            if Cb_Searched.Cb (Cb_Searched.Fd, Cb_Searched.Read) then
              return Fd_Event;
            end if;
          end if;
        exception
          when Cb_Mng.Not_In_List =>
            if Debug then
              Ada.Text_Io.Put_Line ("**** Handle: " & File_Desc'Image(Event.Fd)
                                  & " fd not found ****");
            end if;
        end;
      when Sig_Event =>
        if Cb_Sig /= null then
          Cb_Sig.all;
          return Sig_Event;
        end if;
      when No_Event =>
        -- Nothing. Expire timers or return timeout
        if Timers.Expire then
          return Timer_Event;
        end if;
    end case;
    return No_Event;

  end Handle;

end Event_Mng;

