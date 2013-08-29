with Ada.Exceptions;
with Sys_Calls, Proc_Family, Event_Mng, Trace;

procedure T_Proc_Child is

  Str : String (1 .. 1024);
  Fd_In, Fd_Out, Fd_Err : Sys_Calls.File_Desc;
  Res : Natural;
  First : Boolean := True;

  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Res : Natural;
    use type Sys_Calls.File_Desc;
  begin
    Trace.Put ("Fd cb called", True, True);
    if Fd /= Fd_In then
      Trace.Put ("Fd Cb on invalid Fd", True, True);
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Str'Address, Str'Length);
    if Res = 0 then
      Trace.Put ("Read 0", True, True);
      Event_Mng.Del_Fd_Callback (Fd_In, True);
      return False;
    end if;

    delay 0.1;

    -- This is a write on the Fd to father
    Trace.Put ("Read >" & Str (1 .. Res) & "<", True, True);
    Sys_Calls.Put_Output ("Child: Read >" & Str (1 .. Res) & "<");

    if First then
      -- Unlock the pause
      Trace.Put ("Sending dummy signal", True, True);
      Event_Mng.Send_Dummy_Signal;
      First := False;
    end if;
    Trace.Put ("End of Cb", True, True);
    return False;
  end Fd_Cb;

begin
  Trace.Activate (False);
  Trace.Put ("Starting", True, True);
  begin
    Proc_Family.Child_Get_Fds (Fd_In, Fd_Out, Fd_Err);
  exception
    when Proc_Family.No_Fd =>
      Sys_Calls.Put_Line_Error ("Child: No fd.");
      return;
  end;

  Trace.Put ("Adding fd Cb", True, True);
  Event_Mng.Add_Fd_Callback (Fd_In, True, Fd_Cb'Unrestricted_Access);


  declare
    Msg : constant String := "Child: Fds are " & Fd_In'Img
                       & ", " &  Fd_Out'Img
                       & " and " & Fd_Err'Img;
  begin
    Trace.Put ("Sending >" & Msg &  "<", True, True);
    Res := Sys_Calls.Write (Fd_Out, Msg'Address, Msg'Length);
    if Res /= Msg'Length then
      Trace.Put ("Cannot send", True, True);
      Sys_Calls.Put_Line_Error ("Child: Cannot write "
               & Natural'Image(Msg'Length)
               & " on out fd");
      return;
    end if;
  end;

  Trace.Put ("Pausing", True, True);
  Event_Mng.Pause (Event_Mng.Infinite_Ms);

  Trace.Put ("Pausing 10s", True, True);
  Event_Mng.Pause (10_000);

  Trace.Put ("Closing", True, True);
  Sys_Calls.Close (Fd_In);
  Sys_Calls.Put_Line_Error ("Child: Done.");
  Sys_Calls.Close (Fd_Out);
  Sys_Calls.Close (Fd_Err);
  Trace.Put ("Done", True, True);

exception
  when Error:others =>
   Sys_Calls.Put_Line_Error ("Exception: "
                    & Ada.Exceptions.Exception_Name (Error)
                    & " raised.");
end T_Proc_Child;

