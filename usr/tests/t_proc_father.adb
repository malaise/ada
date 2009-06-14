with Ada.Text_Io;
with Sys_Calls, Proc_Family, Many_Strings, Argument, Text_Handler, Event_Mng;

procedure T_Proc_Father is

  Str : Text_Handler.Text(1024);
  Spawn_Result : Proc_Family.Spawn_Result_Rec;

  procedure Death_Cb (Death_Report : in Proc_Family.Death_Rec) is
    use type Sys_Calls.Death_Cause_List;
  begin
    if Death_Report.Cause = Sys_Calls.Exited then
      Ada.Text_Io.Put_Line ("Father: child pid " & Death_Report.Exited_Pid'Img
        & " has exited with code " & Death_Report.Exit_Code'Img);
    else
      Ada.Text_Io.Put_Line ("Father: child pid " & Death_Report.Signaled_Pid'Img
        & " has exited on signal " & Death_Report.Signal'Img);
    end if;
  end Death_Cb;

  Done : Boolean := False;

  procedure Term_Cb is
  begin
    Ada.Text_Io.Put_Line ("Father: aborted by user");
    Done := True;
  end Term_Cb;

  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Buf : String (1 .. 1024);
    Res : Natural;
    Reply : constant String := "F2C";
    use type Sys_Calls.File_Desc;
  begin
    if Fd /= Spawn_Result.Fd_Out then
      Ada.Text_Io.Put_Line ("Father: Fd Cb on invalid Fd");
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Buf'Address, Buf'Length);
    if Res = 0 then
      Ada.Text_Io.Put_Line ("Father: Read 0");
      Event_Mng.Del_Fd_Callback (Spawn_Result.Fd_Out, True);
      return False;
    end if;
    Ada.Text_Io.Put_Line ("Father: Read >" & Buf(1 .. Res) & "<");
    Buf(1 .. Reply'Length) := Reply;
    begin
      Res := Sys_Calls.Write (Spawn_Result.Fd_In, Buf'Address, Reply'Length);
    exception
      when Sys_Calls.System_Error =>
        Res := 0;
    end;
    if Res /= Reply'Length then
      Ada.Text_Io.Put_Line ("Father: Cannot write "
                            & Natural'Image(Reply'Length)
                            & " bytes on In fd");
    end if;
    return False;
  end Fd_Cb;

  I_Am_Father : Boolean;

begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if I = 1 then
      Text_Handler.Set (Str, Argument.Get_Parameter (Occurence => I));
    else
      Many_Strings.Cat (Str, Argument.Get_Parameter (Occurence => I));
    end if;
  end loop;


  Spawn_Result := Proc_Family.Spawn (Text_Handler.Value(Str),
                                     Proc_Family.Std_Fds,
                                     Death_Cb'Unrestricted_Access);
  if not Spawn_Result.Ok then
    Ada.Text_Io.Put_Line ("Father: Spawn result NOT OK");
    return;
  end if;

  declare
    use type Sys_Calls.Pid;
  begin
    I_Am_Father := Sys_Calls.Get_Pid /= Spawn_Result.Child_Pid;
    if I_Am_Father then
      Ada.Text_Io.Put_Line ("Father: I am father of "
                          & Spawn_Result.Child_Pid'Img);
    else
      Ada.Text_Io.Put_Line ("Father: I am child "
                           & Spawn_Result.Child_Pid'Img);
    end if;
  end;

  if not Spawn_Result.Open then
    Ada.Text_Io.Put_Line ("Father: Spawn result NOT OPEN");
    return;
  end if;

  Ada.Text_Io.Put_Line ("Father: Fds are " & Spawn_Result.Fd_In'Img
                     & ", " & Spawn_Result.Fd_Out'Img
                     & " and " & Spawn_Result.Fd_Err'Img);

  if not I_Am_Father then
    Ada.Text_Io.Put_Line ("Father: Child exiting");
    return;
  end if;

  Event_Mng.Set_Sig_Term_Callback (Term_Cb'Unrestricted_Access);
  Event_Mng.Add_Fd_Callback (Spawn_Result.Fd_Out, True,
                             Fd_Cb'Unrestricted_Access);

  loop
    Event_Mng.Pause (Event_Mng.Infinite_Ms);
    exit when Done;
  end loop;

end T_Proc_Father;

