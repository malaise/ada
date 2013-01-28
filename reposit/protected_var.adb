package body Protected_Var is

  -- Exported operations
  procedure Set (Var : in out Protected_T; Val : in T) is
  begin
    Var.Mutex.Get (Mutex_Manager.Write);
    Var.Val := Val;
    Var.Mutex.Release;
  end Set;

  function Get (Var : Protected_T) return T is
    Res : T;
  begin
    Var.Mutex.Get;
    Res := Var.Val;
    Var.Mutex.Release;
    return Res;
  end Get;

end Protected_Var;

