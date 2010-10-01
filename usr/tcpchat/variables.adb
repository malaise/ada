with Computer, Environ;
with Error;
package body Variables is

  -- Dummy variable resolver for check
  function Dummy (Name : String) return String is
  begin
    return Name;
  end Dummy;

  -- Real ENV variable resolver for expansion
  function Getenv (Name : String) return String is
  begin
    return Environ.Getenv_If_Set (Name);
  exception
    when Environ.Name_Error =>
      Error ("ENV variable " & Name & " not found");
      raise Expand_Error;
  end Getenv;

  -- Reset all variables
  procedure Reset is
  begin
    Computer.Reset (Not_Persistent => False);
  end Reset;

  -- Set a variable
  procedure Set (Name, Value : in Asu_Us) is
  begin
    Computer.Set (Asu_Ts (Name), Asu_Ts (Value),
                  Modifiable => True, Persistent => True);
  exception
    when Computer.Invalid_Variable =>
      raise Invalid_Name;
  end Set;

  -- Set a volatile variable : Clean all volatile variables
  procedure Set_Volatile (Name, Value : in Asu_Us) is
begin
    Computer.Set (Asu_Ts (Name), Asu_Ts (Value),
                  Modifiable => True, Persistent => False);
  exception
    when Computer.Invalid_Variable =>
      raise Invalid_Name;
  end Set_Volatile;

  procedure Clear_Volatiles is
  begin
    Computer.Reset (Not_Persistent => True);
  end Clear_Volatiles;

  -- Expand the expression
  function Expand (Text : Asu_Us;
                   Check_Only : Boolean := False) return String is
  begin
    return Asu_Ts (Expand (Text, Check_Only));
  end Expand;

  function Expand (Text : Asu_Us;
                   Check_Only : Boolean := False) return Asu_Us is
  begin
    if Check_Only then
      Computer.External_Resolver := Dummy'Access;
    else
      Computer.External_Resolver := Getenv'Access;
    end if;
    return Asu_Tus (Computer.Eval (Asu_Ts (Text)));
  exception
    when others =>
      Error ("Cannot expand " & Asu_Ts (Text));
      raise Expand_Error;
  end Expand;

end Variables;

