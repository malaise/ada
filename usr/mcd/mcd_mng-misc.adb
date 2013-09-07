with Ada.Calendar;
with Regular_Expressions, Basic_Proc, Sys_Calls, Trace;

separate (Mcd_Mng)
package body Misc is

  -- Max len for getenv
  Max_Env_Len : constant := 10240;

  procedure Do_Call is
  begin
    Debug.Log (Debug.Oper, "Do_call");
    Stack.Pop(A);
    if A.Kind /= Prog then
      raise Invalid_Argument;
    end if;
    if Call_Stack.Level /= 0 then
      -- Save context;
      Call_Entry := As.U.Tus (Input_Dispatcher.Get_Remaining);
      -- Even if end of subprog, this is not stdin
      if Call_Entry.Length = 0 then
        Call_Entry := As.U.Tus (" ");
      end if;
      Call_Stack.Push (Call_Entry);
    else
      -- Dummy context
      Call_Stack.Push (As.U.Asu_Null);
    end if;
    -- Call
    if A.Val_Text.Is_Null then
      -- Empty subprogram : not stdin
      Input_Dispatcher.Set_Input(" ");
    else
      Input_Dispatcher.Set_Input (A.Val_Text.Image);
    end if;
    S := A;
  end Do_Call;


  procedure Do_Popn is
    N : Natural;
  begin
    Stack.Pop(A);
    -- Has to be Inte and val Natural
    begin
      N := Natural(A.Val_Inte);
    exception
      when others => raise Invalid_Argument;
    end;
    S := A;
    for I in 1 .. N loop
      Stack.Pop(A);
    end loop;
  end Do_Popn;

  procedure Do_Readn is
    N : Positive;
    Rec : Item_Rec;
  begin
    Stack.Pop(A);
    -- Has to be Inte and val positive
    begin
      N := Positive(A.Val_Inte);
    exception
      when others => raise Invalid_Argument;
    end;
    S := A;
    Stack.Readn (Rec, N);
    Stack.Push (Rec);
  end Do_Readn;

  procedure Do_Moven is
    N : Positive;
    Rec : Item_Rec;
  begin
    Stack.Pop(A);
    -- Has to be Inte and val positive
    begin
      N := Positive(A.Val_Inte);
    exception
      when others => raise Invalid_Argument;
    end;
    S := A;
    Stack.Getn (Rec, N);
    Stack.Push (Rec);
  end Do_Moven;


  procedure Do_Clear_Extra is
    Rec : Item_Rec;
  begin
   for I in 1 .. Stack.Stack_Size (Default_Stack => False) loop
     Stack.Pop (Rec, Default_Stack => False);
   end loop;
  end Do_Clear_Extra;

  procedure Do_Rotate_Extra (First : in Boolean; Times : in Item_Rec) is
    Rec : Item_Rec;
  begin
    if Times.Kind /= Inte or else Times.Val_Inte < 0 then
      raise Mcd_Mng.Invalid_Argument;
    end if;
    if Stack.Stack_Size (Default_Stack => False) = 0  then
      return;
    end if;
    for I in 1 .. Times.Val_Inte loop
      if First then
        Stack.Popfe (Rec);
        Stack.Push (Rec, Default_Stack => False);
      else
        Stack.Pop (Rec, Default_Stack => False);
        Stack.Pushfe (Rec);
      end if;
    end loop;
  end Do_Rotate_Extra;

  function Do_Delay (The_Delay : Duration) return Boolean is
    Expiration : Ada.Calendar.Time;
    Timeout_Ms : Integer;
    use type Ada.Calendar.Time, Event_Mng.Out_Event_List;

    procedure Compute_Timeout is
    begin
      Timeout_Ms := Integer ((Expiration - Ada.Calendar.Clock) * 1000);
      if Timeout_Ms < 0 then
        Timeout_Ms := 0;
      end if;
    end Compute_Timeout;

  begin
    Expiration := Ada.Calendar.Clock + The_Delay;
    Compute_Timeout;

    loop
      if Event_Mng.Wait (Timeout_Ms) = Event_Mng.Signal_Event then
        return True;
      end if;
      Compute_Timeout;
      exit when Timeout_Ms = 0;
    end loop;
    return False;
  end Do_Delay;

  function Check_Break return Boolean is
  begin
    return Do_Delay (0.0);
  end Check_Break;

  function Do_Delay (The_Delay : Item_Rec) return Boolean is
  begin
    if The_Delay.Kind = Inte then
      return Do_Delay (Duration(The_Delay.Val_Inte));
    elsif The_Delay.Kind = Real then
      return Do_Delay (Duration(The_Delay.Val_Real));
    else
      raise Invalid_Argument;
    end if;
  end Do_Delay;

  procedure Set_Debug (Set : in Item_Rec) is
  begin
    if Set.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    for L in Debug.Loggers'Range loop
      if Set.Val_Bool then
        Debug.Loggers(L).Add_Mask (Trace.Debug);
      else
        Debug.Loggers(L).Del_Mask (Trace.Debug);
      end if;
    end loop;
  end Set_Debug;

  function Reg_Match (Pattern, Str : Item_Rec) return Item_Rec is
    Res : Regular_Expressions.Match_Cell;
    use type Regular_Expressions.Match_Cell;
  begin
    if Pattern.Kind /= Chrs or else Str.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Res := Regular_Expressions.Match (Pattern.Val_Text.Image,
                                      Str.Val_Text.Image);
    if Res = Regular_Expressions.No_Match then
      return (Kind => Inte, Val_Inte => 0);
    else
      return (Kind => Inte, Val_Inte => My_Math.Inte(Res.First_Offset));
    end if;
  exception
    when Regular_Expressions.No_Criteria =>
      raise Invalid_Argument;
  end Reg_Match;

  Env_Str : String (1 .. Max_Env_Len);
  function Getenv (Item : Item_Rec) return Item_Rec is
    Len : Natural;
    Set, Trunc : Boolean;

  begin
    if Item.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Len := Env_Str'Length;
    Sys_Calls.Getenv (Item.Val_Text.Image, Set, Trunc,
                      Env_Str, Len);
    if not Set then
      return (Kind => Bool, Val_Bool => False);
    end if;
    return (Kind => Chrs,
            Val_Text => As.U.Tus (Env_Str(1 .. Len)));
  end Getenv;

  procedure Set_Exit_Code (Code : Item_Rec) is
  begin
    if Code.Kind /= Inte
    or else Code.Val_Inte < 0
    or else Code.Val_Inte > My_Math.Inte(Natural'Last) then
      raise Invalid_Argument;
    end if;
    Basic_Proc.Set_Exit_Code (Natural(Code.Val_Inte) );
  exception
    when others =>
      raise Invalid_Argument;
  end Set_Exit_Code;

end Misc;

