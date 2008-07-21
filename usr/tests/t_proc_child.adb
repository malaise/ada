with Ada.Text_Io;
with Sys_Calls, Proc_Family, Many_Strings, Argument, Text_Handler, Event_Mng;

procedure T_Proc_Child is

  Str : String (1 .. 1024);
  Fd_In, Fd_Out, Fd_Err : Sys_Calls.File_Desc;
  Msg: constant String := "Child 2 Father";
  Res : Natural;

  function Fd_Cb (Fd : in Sys_Calls.File_Desc;
                  Read : in Boolean) return Boolean is
    Res : Natural;
    use type Sys_Calls.File_Desc;
  begin
    if Fd /= Fd_In then
      Ada.Text_Io.Put_Line ("Child: Fd Cb on invalid Fd");
      return False;
    end if;
    Res := Sys_Calls.Read (Fd, Str'Address, Str'Length);
    if Res = 0 then
      Ada.Text_Io.Put_Line ("Child: Read 0");
      Event_Mng.Del_Fd_Callback (Fd_In, True);
      return False;
    end if;
    Ada.Text_Io.Put_Line ("Child: Read >" & Str (1 .. Res) & "<");

    Event_Mng.Send_Dummy_Signal;
    return False;
  end Fd_Cb;

begin
  begin
    Proc_Family.Child_Get_Fds (Fd_In, Fd_Out, Fd_Err);
  exception
    when Proc_Family.No_Fd =>
      Ada.Text_Io.Put_Line ("Child: No fd.");
      return;
  end;

  Event_Mng.Add_Fd_Callback (Fd_In, True, Fd_Cb'Unrestricted_Access);
  Ada.Text_Io.Put_Line ("Child: Fds are " & Fd_In'Img
                       & ", " &  Fd_Out'Img
                       & " and " & Fd_Err'Img);

  Res := Sys_Calls.Write (Fd_Out, Msg'Address, Msg'Length);
  if Res /= Msg'Length then
    Ada.Text_Io.Put_Line ("Child: Cannot write " & Natural'Image(Msg'Length)
                            & " on out fd");
    return;
  end if;

  Event_Mng.Pause (Event_Mng.Infinite_Ms);

  Sys_Calls.Close (Fd_In);
  Sys_Calls.Close (Fd_Out);

  Ada.Text_Io.Put_Line ("Child: Done.");
end T_Proc_Child;

