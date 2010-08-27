with Ada.Exceptions;
with Sys_Calls, Proc_Family, Event_Mng;

procedure T_Proc_Child is

  Str : String (1 .. 1024);
  Fd_In, Fd_Out, Fd_Err : Sys_Calls.File_Desc;
  Msg: constant String := "Child 2 Father";
  Res : Natural;
  First : Boolean := True;

  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Res : Natural;
    use type Sys_Calls.File_Desc;
  begin
    if Fd /= Fd_In then
      Sys_Calls.Put_Line_Error ("Child: Fd Cb on invalid Fd");
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Str'Address, Str'Length);
    if Res = 0 then
      Sys_Calls.Put_Line_Error ("Child: Read 0");
      Event_Mng.Del_Fd_Callback (Fd_In, True);
      return False;
    end if;

    delay 0.1;

    -- This is a write on the Fd to father
    Sys_Calls.Put_Line_Output ("Child: Read >" & Str (1 .. Res) & "<");

    if First then
      -- Unlock the pause
      Event_Mng.Send_Dummy_Signal;
      First := False;
    end if;
    return False;
  end Fd_Cb;

begin
  begin
    Proc_Family.Child_Get_Fds (Fd_In, Fd_Out, Fd_Err);
  exception
    when Proc_Family.No_Fd =>
      Sys_Calls.Put_Line_Error ("Child: No fd.");
      return;
  end;

  Event_Mng.Add_Fd_Callback (Fd_In, True, Fd_Cb'Unrestricted_Access);
  Sys_Calls.Put_Line_Error("Child: Fds are " & Fd_In'Img
                       & ", " &  Fd_Out'Img
                       & " and " & Fd_Err'Img);

  Res := Sys_Calls.Write (Fd_Out, Msg'Address, Msg'Length);
  if Res /= Msg'Length then
    Sys_Calls.Put_Line_Error ("Child: Cannot write "
             & Natural'Image(Msg'Length)
             & " on out fd");
    return;
  end if;

  Event_Mng.Pause (Event_Mng.Infinite_Ms);

  Event_Mng.Pause (60_000);
  Sys_Calls.Close (Fd_In);
  Sys_Calls.Put_Line_Error ("Child: Done.");
  Sys_Calls.Close (Fd_Out);
  Sys_Calls.Close (Fd_Err);

exception
  when Error:others =>
   Sys_Calls.Put_Line_Error ("Exception: "
                    & Ada.Exceptions.Exception_Name (Error)
                    & " raised.");
end T_Proc_Child;

