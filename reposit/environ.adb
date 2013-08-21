with Unchecked_Deallocation;
with Sys_Calls, Lower_Str, Str_Util;
package body Environ is

  -- Don't allocate strings in stack (they may be too long)
  type String_Access is access String;
  procedure Free is new Unchecked_Deallocation (String, String_Access);

  -- Getenv for a String. Returns empty string if not set.
  function Getenv (Env_Name : String) return String is
  begin
    return Sys_Calls.Getenv (Env_Name);
  exception
    when Sys_Calls.Env_Not_Set =>
      return "";
  end Getenv;

  -- Getenv for a String. Raises Name_Error if not set.
  function Getenv_If_Set (Env_Name : String) return String is
  begin
    return Sys_Calls.Getenv (Env_Name);
  exception
    when Sys_Calls.Env_Not_Set =>
      raise Name_Error;
  end Getenv_If_Set;

  -- Getenv for a String. Leave result unchanged if not set or trunc.
  procedure Get_Str (Name : in String; Result : in out String;
                                       Length : out Natural) is
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
      Str_Util.Copy (Ptr.all(1 .. Env_Len), Result);
      Length := Env_Len;
    end if;

    Free (Ptr);
  end Get_Str;

  procedure Get_Us (Name : in String; Result : in out As.U.Asu_Us) is
    Res : As.U.Asu_Us;
  begin
    Res := As.U.Tus (Sys_Calls.Getenv (Name));
    if not Res.Is_Null then
      Result := Res;
    end if;
  exception
     when Sys_Calls.Env_Not_Set =>
       -- Leave Result unchanged
       null;
  end Get_Us;

  -- Getenv an Integer
  function  Get_Int (Name : String; Default : Integer) return Integer is
    Str : String (1 .. Integer'Width);
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Len   : Natural;
  begin
    Sys_Calls.Getenv (Name, Env_Set, Env_Trunc, Str, Env_Len);
    return (if Env_Set and then not Env_Trunc then
              Integer'Value (Str(1 .. Env_Len))
            else Default);
  exception
    when Constraint_Error =>
      return Default;
  end Get_Int;

  procedure Get_Int (Name : in String; Result : in out Integer) is
  begin
    Result := Get_Int (Name, Result);
  end Get_Int;

  -- Getenv a Natural
  function  Get_Nat (Name : String; Default : Natural) return Natural is
    I : constant Integer := Get_Int (Name, Default);
  begin
    return (if I in Natural then I else Default);
  end Get_Nat;
  procedure Get_Nat (Name : in String; Result : in out Natural) is
  begin
    Result := Get_Nat (Name, Result);
  end Get_Nat;

  -- Getenv a Positive
  function  Get_Pos (Name : String; Default : Positive) return Positive is
    I : constant Integer := Get_Int (Name, Default);
  begin
    return (if I in Positive then I else Default);
  end Get_Pos;

  procedure Get_Pos (Name : in String; Result : in out Positive) is
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
    return (if Env_Set and then not Env_Trunc then
              Pos_Duration'Value (Str(1 .. Env_Len))
            else Default);
  exception
    when Constraint_Error =>
      return Default;
  end Get_Dur;

  procedure Get_Dur (Name : in String; Result : in out Pos_Duration) is
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
    Str : As.U.Asu_Us;
  begin
    Get_Us (Name, Str);
    Str := As.U.Tus (Lower_Str (Str.Image));
    return  Str.Image = "yes" or else Str.Image = "y";
  end Is_Yes;

  -- Is variable set and its lower case is "n" or "no"
  function Is_No (Name : String) return Boolean is
    Str : As.U.Asu_Us;
  begin
    Get_Us (Name, Str);
    Str := As.U.Tus (Lower_Str (Str.Image));
    return  Str.Image = "no" or else Str.Image = "n";
  end Is_No;

end Environ;

