with Ada.Text_Io, Ada.Exceptions, Ada.Characters.Latin_1;
with Event_Mng, Async_Stdin, Rnd, Argument;
with Dictio_Lib;
procedure T_Dictio is

  Init : Boolean := False;
  Verbose : Boolean := False;

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
    if Str'Length = 2 and then Str(1) = 'q' then
      Ada.Text_Io.Put_Line ("CLIENT: Quit");
      Sig := True;
      return True;
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

  procedure Load;

  procedure Dictio_State_Cb (State : in Dictio_Lib.Dictio_State_List) is
    use type Dictio_Lib.Dictio_State_List;
  begin
    if not Init then
      Ada.Text_Io.Put_Line("CLIENT: Dictio state is " & State'Img);
    end if;
    if State /= Dictio_Lib.Unavailable and then Init then
      Init := False;
      Load;
      Sig := True;
    end if;
  end Dictio_State_Cb;

  procedure Dictio_Notify_Cb (Name : in String; Data : in String) is
  begin
    Ada.Text_Io.Put_Line("CLIENT: Notified on >"
                   & Name & "< - >" & Data & "<");
  end Dictio_Notify_Cb;
  
  procedure Load is
    Name : String (1 .. 10);
    N : Positive;
  begin
    for I in 1 .. 100 loop
      N := Rnd.Int_Random (1, Name'Last);
      for J in 1 .. N loop
        Name(J) := Character'Val(Rnd.Int_Random (
              Character'Pos('a'), Character'Pos('z')));
      end loop;
      if Verbose then
        Ada.Text_Io.Put_Line ("CLIENT: Initializing " & Name(1..N));
      end if;
      Dictio_Lib.Set (Name(1..N), "Init_" & Name(1..N));
    end loop;
  end Load;

begin
  begin
    if Argument.Get_Parameter (1, "i") = "" then
      Init := True;
    else
      Init := False;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Init := False;
  end;
  begin
    if Argument.Get_Parameter (1, "v") = "" then
      Verbose := True;
    else
      Verbose := False;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Verbose := False;
  end;

  -- Set async stdin
  begin
    Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access,
                           Async_Stdin.Max_Chars_Range'Last);
  exception
    when Async_Stdin.Not_A_Tty =>
      Ada.Text_Io.Put_Line("CLIENT: Cannot set stdin async");
      return;
  end;

  Event_Mng.Set_Sig_Callback (Sig_Cb'Unrestricted_Access);

  Rnd.Randomize;

  Dictio_Lib.Init;
  Dictio_Lib.Dictio_State_Cb := Dictio_State_Cb'Unrestricted_Access;
  Dictio_Lib.Notify_Cb := Dictio_Notify_Cb'Unrestricted_Access;

  if not Init then
    Ada.Text_Io.Put_Line (
           "g <name> / s <name> <data> / n <name> / c <name> / q");
  end if;
  loop
    Event_Mng.Wait (-1);
    exit when Sig;
  end loop;

  Async_Stdin.Set_Async;
exception
  when others =>
    Async_Stdin.Set_Async;
    raise;
end T_Dictio;

