with Sys_Calls;
package body Key_Pressed is

  Stdin_Is_A_Tty : Boolean := False;

  -- Prepare stdin for silent input
  procedure Open (Blocking : in Boolean) is
    Res : Boolean;
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    Stdin_Is_A_Tty :=
          Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
    if not Stdin_Is_A_Tty then
      return;
    end if;
    if Blocking then
      Res := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Char_No_Echo);
    else
      Res := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Transparent);
    end if;
    if not Res then
      raise Error;
    end if;
  end Open;

  -- Restore Stdin for synchronous input with echo
  procedure Close (Check : in Boolean := False) is
    Res : Boolean;
  begin
    if not Stdin_Is_A_Tty then
      return;
    end if;
    Res := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Canonical);
    if not Res and then Check then
      raise Error;
    end if;
  end Close;

  -- Check if a key has been pressed,
  --  return specific characters if not or on error
  -- No_Key     : constant Character := Ada.Characters.Latin_1.Nul;

  -- Error_Key  : constant Character := Ada.Characters.Latin_1.Eot;

  function Get_Key return Character is
    Status : Sys_Calls.Get_Status_List;
    Result : Character;
    use type Sys_Calls.Get_Status_List;
  begin
    if not Stdin_Is_A_Tty then
      return No_Key;
    end if;
    Sys_Calls.Get_Immediate (Sys_Calls.Stdin, Status, Result);
    if Status = Sys_Calls.None then
      Result := No_Key;
    elsif Status = Sys_Calls.Closed or else Status = Sys_Calls.Error then
      Result := Error_Key;
    end if;
    return Result;
  end Get_Key;

  -- Check if a key has been pressed,
  --  raise exception on error
  -- Error : exception;
  function Key_Pressed return Boolean is
    C : Character;
  begin
    C := Get_Key;
    if C = Error_Key then
      raise Error;
    else
      return C /= No_Key;
    end if;
  end Key_Pressed;

end Key_Pressed;

