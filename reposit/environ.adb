with Unchecked_Deallocation;
with Sys_Calls, Lower_Str;
package body Environ is

  -- Don't allocate strings in stack (they may be too long)
  type String_Access is access String;
  procedure Free is new Unchecked_Deallocation (String, String_Access);

  -- Getenv for a String. Leave result unchanged if not set or trunc.
  procedure Get_Str (Name : String; Result : in out String;
                                    Length : in out Natural) is
    Ptr : String_Access;
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;

  begin
    if Result'Length = 0 then
      Length := 0;
      return;
    end if;
    Ptr := new String(1 .. Result'Length);

    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Ptr.all, Env_Len);
    if Env_Set and then not Env_Trunc and then Env_Len /= 0 then
      Result(1 .. Env_Len) := Ptr.all(1 .. Env_Len);
      Length := Env_Len;
    end if;

    Free (Ptr);
  end Get_Str;


  procedure Get_Txt (Name : String; Result : in out Text_Handler.Text) is
    Ptr : String_Access;
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;
  begin
    if Result.Max_Len = 0 then
      return;
    end if;
    Ptr := new String(1 .. Result.Max_Len);

    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Ptr.all, Env_Len);
    if Env_Set and then not Env_Trunc and then Env_Len /= 0 then
      Text_Handler.Set (Result, Ptr.all(1 .. Env_Len));
    end if;

    Free (Ptr);
  end Get_Txt;

  -- Getenv an Integer
  function  Get_Int (Name : String; Default : Integer) return Integer is
    Str : String (1 .. Integer'Width);
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;
  begin
    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Str, Env_Len);
    if Env_Set and then not Env_Trunc then
      return Integer'Value (Str(1 .. Env_Len));
    else
      return Default;
    end if;
  exception
    when Constraint_Error =>
      return Default;
  end Get_Int;

  procedure Get_Int (Name : String; Result : in out Integer) is
  begin
    Result := Get_Int (Name, Result);
  end Get_Int;

  -- Getenv a Natural
  function  Get_Nat (Name : String; Default : Natural) return Natural is
    I : constant Integer := Get_Int (Name, Default);
  begin
    if I in Natural then
      return I;
    else
      return Default;
    end if;
  end Get_Nat;
  procedure Get_Nat (Name : String; Result : in out Natural) is
  begin
    Result := Get_Nat (Name, Result);
  end Get_Nat;

  -- Getenv a Positive
  function  Get_Pos (Name : String; Default : Positive) return Positive is
    I : constant Integer := Get_Int (Name, Default);
  begin
    if I in Positive then
      return I;
    else
      return Default;
    end if;
  end Get_Pos;

  procedure Get_Pos (Name : String; Result : in out Positive) is
  begin
    Result := Get_Pos (Name, Result);
  end Get_Pos;

  -- Getenv a Duration (positive or null)
  function  Get_Dur (Name : String; Default : Pos_Duration)
            return Pos_Duration is
    Str : String (1 .. Pos_Duration'Width);
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;
  begin
    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Str, Env_Len);
    if Env_Set and then not Env_Trunc then
      return Pos_Duration'Value (Str(1 .. Env_Len));
    else
      return Default;
    end if;
  exception
    when Constraint_Error =>
      return Default;
  end Get_Dur;

  procedure Get_Dur (Name : String; Result : in out Pos_Duration) is
  begin
    Result := Get_Dur (Name, Result);
  end Get_Dur;

  -- Is variable set
  function Is_Set (Name : String) return Boolean is
    Str : String (1 .. 1);
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;
  begin
    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Str, Env_Len);
    return Env_Set;
  end Is_Set;

  -- Is variable set and its lower case is "y" or "yes"
  function Is_Yes (Name : String) return Boolean is
    Txt : Text_Handler.Text(3);
  begin
    Text_Handler.Set (Txt, "-");
    Get_Txt (Name, Txt);
    return  Lower_Str (Text_Handler.Value (Txt)) = "yes"
    or else Lower_Str (Text_Handler.Value (Txt)) = "y";
  end Is_Yes;

  -- Is variable set and its lower case is "n" or "no"
  function Is_No (Name : String) return Boolean is
    Txt : Text_Handler.Text(2);
  begin
    Text_Handler.Set (Txt, "-");
    Get_Txt (Name, Txt);
    return  Lower_Str (Text_Handler.Value (Txt)) = "no"
    or else Lower_Str (Text_Handler.Value (Txt)) = "n";
  end Is_No;

end Environ;

