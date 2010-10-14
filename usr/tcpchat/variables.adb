with Computer, Environ, Regular_Expressions;
with Error;
package body Variables is

  Ext_Ref : constant Character := '$';

  -- Dummy variable resolver for check
  function Dummy (Name : String) return String is
  begin
    return Name;
  end Dummy;

  -- Real ENV variable resolver for expansion
  function Getenv (Name : String) return String is
  begin
    -- Name must start with '$'
    if Name = "" then
      Error ("Empty ENV variable");
      raise Expand_Error;
    end if;
    if Name(Name'First) /= Ext_Ref then
      Error ("Internal variable " & Name & " not found");
      raise Expand_Error;
    end if;
    -- Get ENV variable
    declare
      Str : constant String
          := Name (Integer'Succ(Name'First) .. Name'Last);
    begin
      return Environ.Getenv_If_Set (Str);
    exception
      when Environ.Name_Error =>
        Error ("ENV variable " & Str & " not found");
        raise Expand_Error;
    end;
  end Getenv;

  -- Reset all variables
  procedure Reset is
  begin
    Computer.Reset (Not_Persistent => False);
  end Reset;

  -- Set a variable
  procedure Set (Name, Value : in Asu_Us) is
  begin
    -- Must not be empty, start by '$' or be a number
    if Asu_Is_Null (Name)
    or else Asu.Element (Name, 1) = Ext_Ref
    or else Regular_Expressions.Match ("[0-9]+", Asu_Ts (Name), True) then
      Error ("Invalid variable name" & Asu_Ts (Name));
      raise Invalid_Name;
    end if;
    Computer.Set (Asu_Ts (Name), Asu_Ts (Value),
                  Modifiable => True, Persistent => True);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid variable name" & Asu_Ts (Name));
      raise Invalid_Name;
  end Set;

  -- Set a volatile variable : Clean all volatile variables
  procedure Set_Volatile (Name, Value : in Asu_Us) is
  begin
    -- Must be a number
    if not Regular_Expressions.Match ("[0-9]+", Asu_Ts (Name), True) then
      Error ("Invalid volatile variable name" & Asu_Ts (Name));
      raise Invalid_Name;
    end if;
    Computer.Set (Asu_Ts (Name), Asu_Ts (Value),
                  Modifiable => True, Persistent => False);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid volatile variable name" & Asu_Ts (Name));
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

