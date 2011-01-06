with Computer, Environ, Regular_Expressions, Int_Image;
with Error, Debug;
package body Variables is

  Ext_Ref : constant Character := '$';
  function Image is new Int_Image (Integer);

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
    Debug.Log ("Resetting variables");
    Computer.Reset (Not_Persistent => False);
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
    Computer.Set (Name.Image, Value.Image,
                  Modifiable => True, Persistent => True);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid variable name" & Name.Image);
      raise Invalid_Name;
  end Set;

  function Is_Set (Name : As.U.Asu_Us) return Boolean is
  begin
    Check (Name);
    return Computer.Is_Set (Name.Image);
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
    Computer.Set (Name.Image, Value.Image,
                  Modifiable => True, Persistent => False);
  exception
    when Computer.Invalid_Variable =>
      Error ("Invalid volatile variable name" & Name.Image);
      raise Invalid_Name;
  end Set_Volatile;

  procedure Clear_Volatiles is
  begin
    Debug.Log ("Resetting volatiles");
    Computer.Reset (Not_Persistent => True);
  end Clear_Volatiles;

  -- Expand the expression
  function Expand (Text : As.U.Asu_Us;
                   Check_Only : Boolean := False) return String is
  begin
    return Expand (Text, Check_Only).Image;
  end Expand;

  function Expand (Text : As.U.Asu_Us;
                   Check_Only : Boolean := False) return As.U.Asu_Us is
  begin
    if Check_Only then
      Computer.External_Resolver := Dummy'Access;
    else
      Computer.External_Resolver := Getenv'Access;
    end if;
    return As.U.Tus (Computer.Eval (Text.Image));
  exception
    when others =>
      Error ("Cannot expand " & Text.Image);
      raise Expand_Error;
  end Expand;

  -- Compute a numeric expression
  function Compute (Text : As.U.Asu_Us) return As.U.Asu_Us is
    I : Integer;
  begin
    Computer.External_Resolver := Getenv'Access;
    I := Computer.Compute (Text.Image);
    return As.U.Tus (Image (I));
  exception
    when others =>
      Error ("Cannot compute " & Text.Image);
      raise Expand_Error;
  end Compute;

end Variables;

