with Ada.Text_Io;
with Sys_Calls, Text_Handler, Argument, Many_Strings, Event_Mng;

procedure T_Fork is
  Str : Text_Handler.Text(1024);

  Child : Boolean;
  Child_Pid : Sys_Calls.Pid;

  Done : Boolean := False;

  procedure Sig_Cb is
    Sig_Kind : Event_Mng.Signal_Kind_List;
    Death_Dscr : Sys_Calls.Death_Rec;
    use type Event_Mng.Signal_Kind_List, Sys_Calls.Death_Cause_List;
  begin
    Sig_Kind := Event_Mng.Get_Signal_Kind;
    if Sig_Kind = Event_Mng.Terminate_Sig then
      Ada.Text_Io.Put_Line ("Aborted by user");
      Done := True;
      return;
    elsif Sig_Kind /= Event_Mng.Child_Sig then
      return;
    end if;
    -- Child death
    loop
      Death_Dscr := Sys_Calls.Next_Dead;
      case Death_Dscr.Cause is
        when Sys_Calls.No_Dead =>
          exit;
        when Sys_Calls.Exited =>
          Ada.Text_Io.Put_Line ("Child pid " & Death_Dscr.Exited_Pid'Img
             & " has exited code " &  Death_Dscr.Exit_Code'Img);
        when Sys_Calls.Signaled =>
          Ada.Text_Io.Put_Line ("Child pid " & Death_Dscr.Signaled_Pid'Img
             & " has exited on signal " &  Death_Dscr.Signal'Img);
        when Sys_Calls.Stopped =>
          Ada.Text_Io.Put_Line ("Child pid " & Death_Dscr.Stopped_Pid'Img
             & " has been stopped");
      end case;
    end loop;

  end Sig_Cb;

begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if I = 1 then
      Text_Handler.Set (Str, Argument.Get_Parameter (Occurence => I));
    else
      Many_Strings.Cat (Str, Argument.Get_Parameter (Occurence => I));
    end if;
  end loop;

  -- Fork
  Sys_Calls.Procreate (Child, Child_Pid);

  if Child then
    Ada.Text_Io.Put_Line ("I am child  pid " & Child_Pid'Img
         & " of father pid " & Sys_Calls.Get_Parent_Pid'Img);
    Sys_Calls.Mutate (Text_Handler.Value(Str));
    Ada.Text_Io.Put_Line ("Child mutation has failed!");
  else

    Ada.Text_Io.Put_Line ("I am father pid " & Sys_Calls.Get_Pid'Img
         & " of child  pid " & Child_Pid'Img);

    Event_Mng.Set_Sig_Callback (Sig_Cb'Unrestricted_Access);

    loop
      Event_Mng.Pause (Event_Mng.Infinite_Ms);
      exit when Done;
    end loop;

  end if;

exception
  when Sys_Calls.System_Error =>
    Ada.Text_Io.Put_Line ("Exception System_Error raised");
    raise;
end T_Fork;

