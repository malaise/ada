package body Protected_Var is

  -- Set and Get value Val in Vr
  -- Infinite wait for Get and Release
  procedure Set (Var : in out Protected_T; Val : in T) is
  begin
    Var.Mutex.Get (Mutex_Manager.Write);
    Var.Val := Val;
    Var.Mutex.Release;
  end Set;

  function Get (Var : in out Protected_T) return T is
    Res : T;
  begin
    Var.Mutex.Get;
    Res := Var.Val;
    Var.Mutex.Release;
    return Res;
  end Get;

end Protected_Var;

