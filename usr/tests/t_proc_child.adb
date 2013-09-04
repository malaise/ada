with Ada.Exceptions;
with Sys_Calls, Proc_Family, Event_Mng, Trace;

procedure T_Proc_Child is

  Str : String (1 .. 1024);
  Fd_In, Fd_Out, Fd_Err : Sys_Calls.File_Desc;
  Res : Natural;
  First : Boolean := True;
  Logger : Trace.Logger;

  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    pragma Unreferenced (Read);
    Res : Natural;
    use type Sys_Calls.File_Desc;
  begin
    Logger.Log_Debug ("Fd cb called");
    if Fd /= Fd_In then
      Logger.Log_Debug ("Fd Cb on invalid Fd");
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Str'Address, Str'Length);
    if Res = 0 then
      Logger.Log_Debug ("Read 0");
      Event_Mng.Del_Fd_Callback (Fd_In, True);
      return False;
    end if;

    delay 0.1;

    -- This is a write on the Fd to father
    Logger.Log_Debug ("Read >" & Str (1 .. Res) & "<");
    Sys_Calls.Put_Output ("Child: Read >" & Str (1 .. Res) & "<");

    if First then
      -- Unlock the pause
      Logger.Log_Debug ("Sending dummy signal");
      Event_Mng.Send_Dummy_Signal;
      First := False;
    end if;
    Logger.Log_Debug ("End of Cb");
    return False;
  end Fd_Cb;

begin
  Logger.Init;
  Logger.Log_Debug ("Starting");
  begin
    Proc_Family.Child_Get_Fds (Fd_In, Fd_Out, Fd_Err);
  exception
    when Proc_Family.No_Fd =>
      Logger.Log_Fatal ("Child: No fd.");
      return;
  end;

  Logger.Log_Debug ("Adding fd Cb");
  Event_Mng.Add_Fd_Callback (Fd_In, True, Fd_Cb'Unrestricted_Access);


  declare
    Msg : constant String := "Child: Fds are " & Fd_In'Img
                       & ", " &  Fd_Out'Img
                       & " and " & Fd_Err'Img;
  begin
    Logger.Log_Debug ("Sending >" & Msg &  "<");
    Res := Sys_Calls.Write (Fd_Out, Msg'Address, Msg'Length);
    if Res /= Msg'Length then
      Logger.Log_Error ("Cannot send");
      Logger.Log_Error ("Child: Cannot write "
               & Natural'Image(Msg'Length)
               & " on out fd");
      return;
    end if;
  end;

  Logger.Log_Debug ("Pausing");
  Event_Mng.Pause (Event_Mng.Infinite_Ms);

  Logger.Log_Debug ("Pausing 10s");
  Event_Mng.Pause (10_000);

  Logger.Log_Debug ("Closing");
  Sys_Calls.Close (Fd_In);
  Sys_Calls.Put_Line_Error ("Child: Done.");
  Sys_Calls.Close (Fd_Out);
  Sys_Calls.Close (Fd_Err);
  Logger.Log_Debug ("Done");

exception
  when Error:others =>
   Sys_Calls.Put_Line_Error ("Exception: "
                    & Ada.Exceptions.Exception_Name (Error)
                    & " raised.");
end T_Proc_Child;

