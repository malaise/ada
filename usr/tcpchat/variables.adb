with Computer, Environ, Regular_Expressions, Integer_Image;
with Error, Debug;
package body Variables is

  Memory : Computer.Memory_Type;

  Ext_Ref : constant Character := '$';

  -- Dummy variable resolver for check
  function Dummy (Name : String) return String is
  begin
    return Name;
  end Dummy;

  -- Variable resolver that forbids ENV variables
  function Local (Name : String) return String is
  begin
    -- Name can start with '$'
    if Name = "" then
      Error ("Empty ENV variable");
    elsif Name(Name'First) /= Ext_Ref then
      Error ("Internal variable " & Name & " not found");
    else
      Error ("ENV variable " & Name & " forbidden");
    end if;
    raise Expand_Error;
    return Name;
  end Local;

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
    Debug.Log ("Resetting variables");
    Memory.Reset (Not_Persistent => False);
  end Reset;

  -- Chek that a name is valid
  procedure Check (Name : in As.U.Asu_Us) is
  begin
    -- Must not be empty, start by '$', contain a '=' or be a number
    if Name.Is_Null
    or else Name.Element (1) = Ext_Ref
    or else Regular_Expressions.Match ("=", Name.Image, False)
    or else Regular_Expressions.Match ("[0-9]+", Name.Image, True) then
      Error ("Invalid variable name" & Name.Image);
      raise Invalid_Name;
    end if;
  end Check;

  -- Set a variable
  procedure Set (Name, Value : in As.U.Asu_Us) is
  begin
    Check (Name);
    Memory.Set (Name.Image, Value.Image,
                Modifiable => True, Persistent => True);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid variable name" & Name.Image);
      raise Invalid_Name;
  end Set;

  function Is_Set (Name : As.U.Asu_Us) return Boolean is
  begin
    Check (Name);
    return Memory.Is_Set (Name.Image);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid variable name" & Name.Image);
      raise Invalid_Name;
  end Is_Set;

  -- Set a volatile variable : Clean all volatile variables
  procedure Set_Volatile (Name, Value : in As.U.Asu_Us) is
  begin
    -- Must be a number
    if not Regular_Expressions.Match ("[0-9]+", Name.Image, True) then
      Error ("Invalid volatile variable name" & Name.Image);
      raise Invalid_Name;
    end if;
    Memory.Set (Name.Image, Value.Image,
                  Modifiable => True, Persistent => False);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid volatile variable name" & Name.Image);
      raise Invalid_Name;
  end Set_Volatile;

  procedure Clear_Volatiles is
  begin
    Debug.Log ("Resetting volatiles");
    Memory.Reset (Not_Persistent => True);
  end Clear_Volatiles;

  -- Expand the expression
  function Expand (Text : As.U.Asu_Us;
                   Exp_Mode : Exp_Mode_List) return String is
  begin
    return Expand (Text, Exp_Mode).Image;
  end Expand;

  function Expand (Text : As.U.Asu_Us;
                   Exp_Mode : Exp_Mode_List) return As.U.Asu_Us is
  begin
    case Exp_Mode is
      when Check_Only =>
        Memory.Set_External_Resolver (Dummy'Access);
      when Local_Only =>
        Memory.Set_External_Resolver (Local'Access);
      when Local_Env =>
        Memory.Set_External_Resolver (Getenv'Access);
    end case;
    return As.U.Tus (Memory.Eval (Text.Image));
  exception
    when others =>
      Error ("Cannot expand " & Text.Image);
      raise Expand_Error;
  end Expand;

  -- Compute a numeric expression
  function Compute (Text : As.U.Asu_Us) return As.U.Asu_Us is
    I : Integer;
  begin
    Memory.Set_External_Resolver (Getenv'Access);
    I := Memory.Compute (Text.Image);
    return As.U.Tus (Integer_Image (I));
  exception
    when others =>
      Error ("Cannot compute " & Text.Image);
      raise Expand_Error;
  end Compute;

end Variables;

