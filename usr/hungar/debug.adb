with Sys_Calls, Lower_Str;
package body Debug is

  Known : Boolean := False;
  Set : Boolean := False;

  Debug_Var : constant String := "HUNGAR_DEBUG";

  function On return Boolean is
    Env_Set   : Boolean;
    Env_Trunc : Boolean;
    Env_Value : String (1 .. 3);
    Env_Len   : Natural;

  begin
    if not Known then
      Sys_Calls.Getenv(Debug_Var, Env_Set, Env_Trunc, Env_Value, Env_Len);
      if Env_Set and then not Env_Trunc then
        if Lower_Str(Env_Value(1 .. Env_Len)) = "yes"
        or else Lower_Str(Env_Value(1 .. Env_Len)) = "y" then
          Set := True;
        end if;
      end if;
      Known := True;
    end if;
    return Set;
  end On;

end Debug;

