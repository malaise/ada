with Computer, Environ;
with Debug;
package body Variables is

  -- Check that expression expands correctly (even if some Vars are unknown)
  function Dummy (Name : String) return String is
  begin
    return Name;
  end Dummy;
  procedure Check (Text : in Asu_Us) is
    Res : Asu_Us;
    pragma Unreferenced (Res);
  begin
    Computer.External_Resolver := Dummy'Access;
    Res := Asu_Tus (Computer.Eval (Asu_Ts (Text)));
  exception
    when others =>
      raise Check_Error;
  end Check;

  -- Set a variable
  procedure Set (Name, Value : in Asu_Us) is
  begin
    Computer.Set (Asu_Ts (Name), Asu_Ts (Value),
                  Modifiable => True, Persistent => False);
  end Set;

  -- Reset all variables
  procedure Reset is
  begin
    Computer.Reset (Not_Persistent => True);
  end Reset;

  -- Expand the expression, using defined variables or env variables
  function Getenv (Name : String) return String is
  begin
    return Environ.Getenv_If_Set (Name);
  exception
    when Environ.Name_Error =>
      Debug.Log ("ENV variable " & Name & " not found");
      raise Expand_Error;
  end Getenv;
  function Expand (Text : Asu_Us) return String is
  begin
    return Asu_Ts (Expand (Text));
  end Expand;

  function Expand (Text : Asu_Us) return Asu_Us is
  begin
    Computer.External_Resolver := Getenv'Access;
    return Asu_Tus (Computer.Eval (Asu_Ts (Text)));
  exception
    when others =>
      raise Expand_Error;
  end Expand;

end Variables;

