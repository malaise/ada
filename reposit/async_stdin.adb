with X_Mng, Sys_Calls, Text_Handler;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  Max : Max_Chars_Range := 1;
  Txt : Text_Handler.Text (Max_Chars_Range'Last);

  -- Our callback
  function Fd_Callback (Fd : in Sys_Calls.File_Desc;
                        Read : in Boolean) return Boolean is
    C : Character;
    A : Boolean;
  begin
    loop
      Sys_Calls.Get_Immediate_Stdin (C, A);
      if not A then
        -- No more char
        return False;
      end if;
      -- Dump character code
      -- Sys_Calls.Put_Line_Error (Integer'Image(Character'Pos(C)));
      Text_Handler.Append (Txt, C);
      exit when Text_Handler.Length (Txt) = Max
      or else C < ' ';
    end loop;

    A := Cb(Text_Handler.Value (Txt));
    Text_Handler.Empty (Txt);
    return A;
  end;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1) is
    Result : Boolean;
  begin
    -- Check if restore
    if User_Callback = null then
      if Cb = null then
        return;
      else
        Cb := null;
        Result := Sys_Calls.Set_Stdin_Attr (Sys_Calls.Canonical);
        X_Mng.X_Del_Callback (Sys_Calls.Stdin, True);
      end if;
    else
      if Cb = null then
        Result := Sys_Calls.Set_Stdin_Attr (Sys_Calls.Asynchronous);
      end if;
      if Result then
        Cb := User_Callback;
        Max := Max_Chars;
        X_Mng.X_Add_Callback (Sys_Calls.Stdin, True, Fd_Callback'access);
      else
        raise Not_A_Tty;
      end if;
    end if;
  end Set_Async;

end Async_Stdin;

