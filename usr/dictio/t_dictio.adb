with Ada.Text_Io, Ada.Exceptions, Ada.Characters.Latin_1;
with Text_Handler, Event_Mng, Async_Stdin, Rnd, Argument, Sys_Calls, Parser,
     Lower_Str;
with Dictio_Lib;
procedure T_Dictio is

  Init : Boolean := False;
  Verbose : Boolean := False;
  Saved_State : Dictio_Lib.Dictio_State_List := Dictio_Lib.Unavailable;

  -- Signal received (while loading)
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

  procedure Put_Help is
  begin
    Ada.Text_Io.Put_Line ("Comands are:");
    Ada.Text_Io.Put_Line ("  set    <name> <data>          get      <name>");
    Ada.Text_Io.Put_Line ("  notify <name>                 cancel   <name>");
    Ada.Text_Io.Put_Line ("  alias  <name> <name>          getalias <name>");
    Ada.Text_Io.Put_Line ("  add    <host>                 del      <host>");
    Ada.Text_Io.Put_Line ("  help                          status");
    Ada.Text_Io.Put_Line ("  quit");
  end Put_Help;

  function Is_Sep (C : Character) return Boolean is
  begin
    return C = ' ';
  end Is_Sep;

  function Stdin_Cb (Str : in String) return Boolean is
    Iter : Parser.Iterator;
    Key, Name, Last : Text_Handler.Text (Dictio_Lib.Max_Name_Len);
    Data : Text_Handler.Text (Dictio_Lib.Max_Data_Len);
  begin
    -- Sanity checks and specific codes
    if Str'Length = 0 then
      return True;
    end if;
    if Str'Length >= 1
    and then Str(Str'Length) = Ada.Characters.Latin_1.Etx then
      Ada.Text_Io.Put_Line ("CLIENT: Aborted");
      Event_Mng.Send_Dummy_Signal;
      return True;
    end if;
    if Str(Str'Length) = Ada.Characters.Latin_1.Eot then
      Ada.Text_Io.Put_Line ("CLIENT: Terminated");
      Event_Mng.Send_Dummy_Signal;
      return True;
    end if;
    if Str'Length <= 1
    or else Str(Str'Length) /= Ada.Characters.Latin_1.Lf then
      Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      return False;
    end if;

    -- Parse
    begin
      Parser.Create (Str(1 .. Str'Last-1), Is_Sep'Unrestricted_Access, Iter);
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
    end;
    begin
      Text_Handler.Set (Key,  Lower_Str(Parser.Next_Word(Iter)));
      Text_Handler.Set (Name, Parser.Next_Word(Iter));
      Text_Handler.Set (Data, Parser.Next_Word(Iter));
      Text_Handler.Set (Last, Parser.Next_Word(Iter));
    exception
      when Constraint_Error =>
        Parser.Delete (Iter);
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
    end;
    Parser.Delete (Iter);

    -- Only key: Quit, help or status
    if Text_Handler.Value (Key) = "quit"
    or else Text_Handler.Value (Key) = "help"
    or else Text_Handler.Value (Key) = "status" then
      if not Text_Handler.Empty (Name) then
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
      end if;
      if Text_Handler.Value (Key) = "quit" then
        Ada.Text_Io.Put_Line ("CLIENT: Quit");
        Event_Mng.Send_Dummy_Signal;
        return True;
      elsif Text_Handler.Value (Key) = "help" then
        Put_Help;
      elsif Text_Handler.Value (Key) = "status" then
        Ada.Text_Io.Put_Line("CLIENT: Dictio status is " & Saved_State'Img);
      else
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      end if;
        return False;
    end if;


    -- Key & Name: Get, notify, cancel, unalias, add, del
    if Text_Handler.Value (Key) = "get" 
    or else Text_Handler.Value (Key) = "notify"
    or else Text_Handler.Value (Key) = "cancel"
    or else Text_Handler.Value (Key) = "getalias"
    or else Text_Handler.Value (Key) = "add"
    or else Text_Handler.Value (Key) = "del" then
      if Text_Handler.Empty (Name)
      or else not Text_Handler.Empty (Data) then
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
      end if;
      if Text_Handler.Value (Key) = "get" then
        Ada.Text_Io.Put_Line ("CLIENT: got >"
              & Dictio_Lib.Get (Text_Handler.Value (Name)) & "<");
      elsif Text_Handler.Value (Key) = "notify" then
        Dictio_Lib.Notify (Text_Handler.Value (Name), True);
      elsif Text_Handler.Value (Key) = "cancel" then
        Dictio_Lib.Notify (Text_Handler.Value (Name), False);
      elsif Text_Handler.Value (Key) = "getalias" then
        Ada.Text_Io.Put_Line ("CLIENT: got alias >"
              & Dictio_Lib.Get_Alias (Text_Handler.Value (Name)) & "<");
      elsif Text_Handler.Value (Key) = "add" then
        Dictio_Lib.Add_Host (Text_Handler.Value (Name));
      elsif Text_Handler.Value (Key) = "del" then
        Dictio_Lib.Del_Host (Text_Handler.Value (Name));
      else
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      end if;
      return False;
    end if;

    -- Key & Name [ Data ]: Set or alias
    if Text_Handler.Value (Key) = "set" 
    or else Text_Handler.Value (Key) = "alias" then
      if Text_Handler.Empty (Name)
      or else not Text_Handler.Empty (Last) then
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
        return False;
      end if;
      if Text_Handler.Value (Key) = "set" then
        Dictio_Lib.Set (Text_Handler.Value (Name), Text_Handler.Value (Data));
      elsif Text_Handler.Value (Key) = "alias" then
        Dictio_Lib.Set_Alias (Text_Handler.Value (Name), Text_Handler.Value (Data));
      else
        Ada.Text_Io.Put_Line ("CLIENT: Discarded");
      end if;
      return False;
    end if;

    Ada.Text_Io.Put_Line ("CLIENT: Discarded");
    return False;

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
    Saved_State := State;
    if not Init then
      Ada.Text_Io.Put_Line("CLIENT: Dictio state is " & State'Img);
    end if;
    if State /= Dictio_Lib.Unavailable and then Init then
      Load;
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
      for J in 1 .. 100 loop
        N := Rnd.Int_Random (1, Name'Last);
        for K in 1 .. N loop
          Name(K) := Character'Val(Rnd.Int_Random (
                Character'Pos('a'), Character'Pos('z')));
        end loop;
        if N > 3 and then Rnd.Int_Random = 1 then
          Name(2) := '.';
        end if;
        if Verbose then
          Ada.Text_Io.Put_Line ("CLIENT: Initializing " & Name(1..N));
        end if;
        Dictio_Lib.Set (Name(1..N), "Init_" & Name(1..N));
        exit when Sig;
      end loop;
      Event_Mng.Pause (100);
      exit when Sig;
    end loop;
    Sig := True;
    Event_Mng.Send_Dummy_Signal;
  exception
    when Dictio_Lib.No_Dictio =>
      Ada.Text_Io.Put_Line ("CLIENT.LOAD: No Dictio");
      Sig := True;
      Event_Mng.Send_Dummy_Signal;
      Sys_Calls.Set_Error_Exit_Code;
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
  if not Init then
    begin
      Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access,
                             Async_Stdin.Max_Chars_Range'Last);
    exception
      when Async_Stdin.Error =>
        Ada.Text_Io.Put_Line("CLIENT: Cannot set stdin async");
        return;
    end;
  end if;

  Event_Mng.Set_Sig_Term_Callback (Sig_Cb'Unrestricted_Access);

  Rnd.Randomize;

  Dictio_Lib.Dictio_State_Cb := Dictio_State_Cb'Unrestricted_Access;
  Dictio_Lib.Notify_Cb := Dictio_Notify_Cb'Unrestricted_Access;
  Dictio_Lib.Init;

  if not Init then
    Put_Help;
  end if;
  if not Sig then
    Event_Mng.Pause (Event_Mng.Infinite_Ms);
  end if;

  Async_Stdin.Set_Async;
exception
  when others =>
    Async_Stdin.Set_Async;
    raise;
end T_Dictio;

