with Unchecked_Deallocation;
package body Virtual_Time is

  -- Affection of observer
  procedure Set (To : out Observer_Access; Val : in Observer_Access) is
  begin
    To := Val;
  end Set;

  -- Get current time
  function Current_Time (A_Clock : Clock) return Time is
  begin
    return Virtual_Time_Of (A_Clock, Ada.Calendar.Clock);
  end Current_Time;

  -- Notify observers of a clock change
  procedure Notify_Observers (A_Clock : Clock;
                              Vtime : Time) is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
    Obs : Observer_Access;
  begin
    if A.Observers.Is_Empty then
      return;
    end if;
    A.Observers.Rewind;
    loop
      A.Observers.Read (Obs, List_Mng.Current);
      Obs.all.Notify (Vtime, A_Clock'Unrestricted_Access);
      exit when not A.Observers.Check_Move;
      A.Observers.Move_To;
    end loop;
  end Notify_Observers;

  -- Set a new synchro point
  procedure Set_Time (A_Clock : in Clock;
                      Reference_Time : in Time;
                      Virtual_Time : in Time) is
    Vtime : constant Time := Current_Time (A_Clock);
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
  begin
    A.Refe_Time := Reference_Time;
    A.Virt_Time := Virtual_Time;
    Notify_Observers (A_Clock, Vtime);
  end Set_Time;

   -- Get current synchro point
  procedure Get_Synchro (A_Clock : in Clock;
                         Reference_Time : out Time;
                         Virtual_Time : out Time) is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
  begin
    Reference_Time := A.Refe_Time;
    Virtual_Time := A.Virt_Time;
  end Get_Synchro;

  -- Set a new speed
  procedure Set_Speed (A_Clock : in Clock;
                       Speed : in Speed_Range) is
    Now : constant Time := Ada.Calendar.Clock;
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
  begin
    -- Set synchro point to now (with previous speed)
    A.Virt_Time := Virtual_Time_Of (A_Clock, Now);
    A.Refe_Time := Now;
    -- Set new speed
    A.Speed := Speed;
    Notify_Observers (A_Clock, A.Virt_Time);
  end Set_Speed;

  -- Get current speed
  function Get_Speed (A_Clock : Clock) return Speed_Range is
  begin
    return A_Clock.Clock_Access.Speed;
  end Get_Speed;

  -- Get Virtual time corresponding to a Reference time
  function Virtual_Time_Of (A_Clock : Clock;
                            Reference_Time : Time) return Time is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
    use type Time;
  begin
    -- (Cur_Virt - Ref_Virt) / (Cur_Real - Ref_Real) = Speed
    -- Cur_Virt = (Cur_Real - Ref_Real) * Speed + Ref_Virt
    return (Reference_Time - A.Refe_Time) * A.Speed + A.Virt_Time;
  end Virtual_Time_Of;

  -- Get Reference time corresponding to a Virtual time
  function Reference_Time_Of (A_Clock : Clock;
                              Virtual_Time : Time) return Time is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
    use type Time;
  begin
    -- (Cur_Virt - Ref_Virt) / (Cur_Real - Ref_Real) = Speed
    -- Cur_Real = (Cur_Virt - Ref_Virt) / speed + Ref_Real
    if A.Speed = 0.0 then
      raise Vtime_Error;
    end if;
    return  (Virtual_Time - A.Virt_Time) / A.Speed + A.Refe_Time;
  end Reference_Time_Of;

  -- Add a new observer
  procedure Add_Observer (A_Clock : Clock;
                          An_Observer :  access Observer'Class) is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
  begin
    if not A.Observers.Is_Empty then
      A.Observers.Rewind (List_Mng.Prev);
    end if;
    A.Observers.Insert (Observer_Access(An_Observer));
  end Add_Observer;

  -- Del an observer
  procedure Del_Observer (A_Clock : Clock;
                          An_Observer :  access Observer'Class) is
    A : constant Clock_Def_Access := A_Clock.Clock_Access;
    Obs : Observer_Access;
  begin
    if A.Observers.Is_Empty then
      return;
    end if;
    -- Iterate until this observer is found (and delete it) or
    --  until end of list
    A.Observers.Rewind;
    loop
      A.Observers.Read (Obs, List_Mng.Current);
      if Obs = Observer_Access (An_Observer) then
        -- This observer matches: delete and exit
        if A.Observers.Check_Move then
          A.Observers.Delete (List_Mng.Next);
        else
          A.Observers.Delete (List_Mng.Prev);
        end if;
        exit;
      end if;
      exit when not A.Observers.Check_Move;
      A.Observers.Move_To;
    end loop;
  end Del_Observer;

  -- Finalyze virtual clock: deallocate so that Clock_Def_Rec
  --  (and Observers list) is deallocated
  procedure Deallocate is new Unchecked_Deallocation (
    Object => Clock_Def_Rec,
    Name => Clock_Def_Access);
  procedure Finalize (A_Clock : in out Clock) is
  begin
    Deallocate (A_Clock.Clock_Access);
  end Finalize;

end Virtual_Time;

