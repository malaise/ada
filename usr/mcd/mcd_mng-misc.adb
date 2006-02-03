with Ada.Calendar;
with Event_Mng, Regular_Expressions;

separate (Mcd_Mng)
package body Misc is

  procedure Do_Call is
  begin
    if Debug.Debug_Level_Array(Debug.Oper) then
      Async_Stdin.Put_Line_Err("Mng: Do_call");
    end if;
    Stack.Pop(A);
    if A.Kind /= Prog then
      raise Invalid_Argument;
    end if;
    if Call_Stack.Level /= 0 then
      -- Save contect;
      Call_Entry := Unb.To_Unbounded_String (Input_Dispatcher.Get_Remaining);
      -- Even if end of subprog, this is not stdin
      if Unb.Length (Call_Entry) = 0 then
        Call_Entry := Unb.To_Unbounded_String (" ");
      end if;
      Call_Stack.Push (Call_Entry);
    else
      -- Dummy context
      Call_Stack.Push (Unb.To_Unbounded_String (""));
    end if;
    -- Call
    if Unb.Length (A.Val_Text) = 0 then
      -- Empty subprogram : not stdin
      Input_Dispatcher.Set_Input(" ");
    else
      Input_Dispatcher.Set_Input (Unb.To_String (A.Val_Text));
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
        Stack.Popf (Rec);
        Stack.Push (Rec, Default_Stack => False);
      else
        Stack.Pop (Rec, Default_Stack => False);
        Stack.Pushf (Rec);
      end if;
    end loop;
  end Do_Rotate_Extra;

  function  Do_Delay (The_Delay : Duration) return Delay_Status_List is
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
        return Exit_Break;
      end if;
      Compute_Timeout;
      exit when Timeout_Ms = 0;
    end loop;
    return Continue;
  end Do_Delay;

  function Do_Delay (The_Delay : Item_Rec) return Delay_Status_List is
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
    Debug.Debug_Level_Array := (others => Set.Val_Bool);
  end Set_Debug;

  function Reg_Match (Pattern, Str : Item_Rec) return Item_Rec is
    Criteria : Regular_Expressions.Compiled_Pattern;
    Res : Natural;
  begin
    if Pattern.Kind /= Chrs or else Str.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
    Res := Regular_Expressions.Match (Unb.To_String (Pattern.Val_Text),
                                     Unb.To_String (Str.Val_Text));
    return (Kind => Inte, Val_Inte => My_Math.Inte(Res));
  exception
    when Regular_Expressions.No_Criteria =>
      raise Invalid_Argument;
  end Reg_Match;

end Misc;

