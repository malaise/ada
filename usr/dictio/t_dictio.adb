with Ada.Text_Io, Ada.Exceptions, Ada.Characters.Latin_1;
with X_Mng, Async_Stdin;
with Dictio_Lib;
procedure T_Dictio is

  -- Signal received
  Sig : Boolean := False;
  procedure Sig_Cb is
  begin
    Ada.Text_Io.Put_Line ("CLIENT: Aborted");
    Sig := True;
  end Sig_Cb;

  function Next_Space (In_Str : String; From : in Positive) return Natural is
  begin
    for I in From .. In_Str'Last loop
      if In_Str(I) = ' ' then
        return I;
      end if;
    end loop;
    return 0;
  end Next_Space;

  function Stdin_Cb (Str : in String) return Boolean is
    N : Natural;
    I : Natural;
    Lstr : constant String := Str(1 .. Str'Last - 1);
  begin
    if Str'Length >= 1
    and then Str(Str'Length) = Ada.Characters.Latin_1.Etx then
      Ada.Text_Io.Put_Line ("CLIENT: Aborted");
      Sig := True;
      return True;
    end if;
    if Str(Str'Length) /= Ada.Characters.Latin_1.Lf then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;

    N := 0;
    for I in Lstr'Range loop
      if Lstr(I) = ' ' then
        N := N + 1;
      end if;
    end loop;
    if N /= 1 and then N /= 2 then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;
    I := Next_Space (Lstr, 1);
    if I /= 2 or else Lstr'Length < 3 then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;

    if Lstr(1) = 'g' or else Lstr(1) = 'n' or else Lstr(1) = 'c' then
      if N /= 1 or else I = Lstr'Last then
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
      end if;
      if Lstr(1) = 'g' then
        Ada.Text_Io.Put_Line ("CLIENT: got >"
              & Dictio_Lib.Get (Lstr(3 .. Lstr'Last)) & "<");
      elsif Lstr(1) = 'n' then
        Dictio_Lib.Notify (Lstr(3 .. Lstr'Last), True);
      else
        Dictio_Lib.Notify (Lstr(3 .. Lstr'Last), False);
      end if;
      return False;
    end if;
    if N /= 2 then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;
    I := Next_Space (Lstr, 3);
    if I = 0 or else I = Lstr'Last then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;
    if Lstr(1) = 's' then
      Dictio_Lib.Set (Lstr(3 .. I-1), Lstr(I+1 .. Lstr'Last));
      return False;
    else
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;
  exception
    when Dictio_Lib.No_Dictio =>
      Ada.Text_Io.Put_Line ("CLIENT: No Dictio");
      return False;
    when Dictio_Lib.No_Item =>
      Ada.Text_Io.Put_Line ("CLIENT: No Item");
      return False;
    when Dictio_Lib.Name_Too_Long =>
      Ada.Text_Io.Put_Line ("CLIENT: Name Too Long");
      return False;
    when Dictio_Lib.Data_Too_Long =>
      Ada.Text_Io.Put_Line ("CLIENT: Data Too Long");
      return False;
    when Error:others =>
      Ada.Text_Io.Put_Line ("CLIENT: Exception: "
             & Ada.Exceptions.Exception_Name (Error));
      return False;
  end Stdin_Cb;

  procedure Dictio_Connect_Cb (Connected : in Boolean) is
  begin
    Ada.Text_Io.Put_Line("CLIENT: Connected " & Connected'Img);
  end Dictio_Connect_Cb;

  procedure Dictio_Notify_Cb (Name : in String; Data : in String) is
  begin
    Ada.Text_Io.Put_Line("CLIENT: Notified on >"
                   & Name & "< - >" & Data & "<");
  end Dictio_Notify_Cb;
  

  Res : Boolean;

begin
  -- Set async stdin
  begin
    Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access,
                           Async_Stdin.Max_Chars_Range'Last);
  exception
    when Async_Stdin.Not_A_Tty =>
      Ada.Text_Io.Put_Line("CLIENT: Cannot set stdin async");
      return;
  end;

  X_Mng.X_Set_Signal (Sig_Cb'Unrestricted_Access);

  Dictio_Lib.Init;
  Dictio_Lib.Available_Cb := Dictio_Connect_Cb'Unrestricted_Access;
  Dictio_Lib.Notify_Cb := Dictio_Notify_Cb'Unrestricted_Access;

  Ada.Text_Io.Put_Line ("g <name> / s <name> <data> / n <name> / c <name>");
  loop
    Res := X_Mng.Select_No_X (-1);
    exit when Sig;
  end loop;

  Async_Stdin.Set_Async;
exception
  when others =>
    Async_Stdin.Set_Async;
    raise;
end T_Dictio;

