with Ada.Text_Io;
with Event_Mng, Sys_Calls, Text_Handler;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  Max : Max_Chars_Range := 1;
  Txt : Text_Handler.Text (Max_Chars_Range'Last);

  -- Our callback
  function Fd_Callback (Fd : in Sys_Calls.File_Desc;
                        Read : in Boolean) return Boolean is
    C : Character;
    Status : Sys_Calls.Get_Status_List;
    Result : Boolean;
  begin
    loop
      Sys_Calls.Get_Immediate (Fd, Status, C);
      case Status is
        when Sys_Calls.Got =>
          -- Dump character code
          -- Sys_Calls.Put_Line_Error (Integer'Image(Character'Pos(C)));
          if Character'Pos(C) = 8 or else Character'Pos(C) = 127 then
            -- Backspace
            if not Text_Handler.Empty (Txt) then
              -- Erase prev char and move one left
              Ada.Text_Io.Put (Ascii.Bs & ' ' & Ascii.Bs);
              Text_Handler.Set (Txt,
                Text_Handler.Value(Txt)(1 .. Text_Handler.Length(Txt) - 1));
            end if;
          else
            if C >= ' ' or else C = Ascii.Lf then
              Ada.Text_Io.Put (C);
            end if;
            Text_Handler.Append (Txt, C);
            exit when Text_Handler.Length (Txt) = Max
            or else C < ' ';
          end if;
        when Sys_Calls.None =>
          -- No more char
          return False;
        when Sys_Calls.Closed | Sys_Calls.Error =>
          -- Call Cb with empty txt
          Text_Handler.Empty (Txt);
          exit;
      end case;
    end loop;

    -- Dump result
    -- for I in 1 .. Text_Handler.Length(Txt) loop
    --   Sys_Calls.Put_Line_Error (
    --      Integer'Image(Character'Pos(Text_Handler.Value(Txt)(I))));
    -- end loop;
    Result := Cb (Text_Handler.Value (Txt));
    Text_Handler.Empty (Txt);
    return Result;
  end;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1) is
    Result : Boolean;
    Stdin_Is_A_Tty : Boolean;
    use type  Sys_Calls.File_Desc_Kind_List;
  begin
    Stdin_Is_A_Tty := Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
    -- Check if restore
    if User_Callback = null then
      if Cb = null then
        return;
      else
        Cb := null;
        if Stdin_Is_A_Tty then
          Result := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, 
                                            Sys_Calls.Canonical);
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
        end if;
        Event_Mng.Del_Fd_Callback (Sys_Calls.Stdin, True);
      end if;
    else
      if Cb = null then
        if Stdin_Is_A_Tty then
          Result := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin,
                                            Sys_Calls.Transparent);
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
        end if;
      else
        Result := True;
      end if;
      if Result then
        Cb := User_Callback;
        Max := Max_Chars;
        Text_Handler.Empty (Txt);
        Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True, Fd_Callback'Access);
      else
        raise Error;
      end if;
    end if;
  end Set_Async;

end Async_Stdin;

