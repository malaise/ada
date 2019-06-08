with Basic_Proc, Sys_Calls, Argument, Many_Strings, Event_Mng;

procedure T_Fork is
  Str : Many_Strings.Many_String;

  Child : Boolean;
  Child_Pid : Sys_Calls.Pid;


  procedure Sig_Term_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Aborted by user");
    Basic_Proc.Set_Error_Exit_Code;
  end Sig_Term_Cb;

  procedure Sig_Child_Cb is
    Death_Dscr : Sys_Calls.Death_Rec;
  begin
    loop
      Death_Dscr := Sys_Calls.Next_Dead;
      case Death_Dscr.Cause is
        when Sys_Calls.No_Dead =>
          exit;
        when Sys_Calls.Exited =>
          Basic_Proc.Set_Exit_Code (Death_Dscr.Exit_Code);
          Basic_Proc.Put_Line_Output ("Child pid " & Death_Dscr.Exited_Pid'Img
             & " has exited code " &  Death_Dscr.Exit_Code'Img);
        when Sys_Calls.Signaled =>
          Basic_Proc.Set_Error_Exit_Code;
          Basic_Proc.Put_Line_Output ("Child pid " & Death_Dscr.Signaled_Pid'Img
             & " has exited on signal " &  Death_Dscr.Signal'Img);
        when Sys_Calls.Stopped =>
          Basic_Proc.Set_Error_Exit_Code;
          Basic_Proc.Put_Line_Output ("Child pid " & Death_Dscr.Stopped_Pid'Img
             & " has been stopped");
      end case;
    end loop;
  end Sig_Child_Cb;

begin

  -- Build String (may raise Constraint_Error if args too long)
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Str.Cat (Argument.Get_Parameter (Occurence => I));
  end loop;

  -- Catch signals
  Event_Mng.Set_Sig_Term_Callback (Sig_Term_Cb'Unrestricted_Access);
  Event_Mng.Set_Sig_Child_Callback (Sig_Child_Cb'Unrestricted_Access);

  -- Fork
  Sys_Calls.Procreate (Child, Child_Pid);

  if Child then
    Basic_Proc.Put_Line_Output ("I am child  pid " & Child_Pid'Img
         & " of father pid " & Sys_Calls.Get_Parent_Pid'Img);
    Sys_Calls.Mutate (Str);
    Basic_Proc.Put_Line_Error ("Child mutation has failed!");
    Basic_Proc.Set_Error_Exit_Code;
  else

    Basic_Proc.Put_Line_Output ("I am father pid " & Sys_Calls.Get_Pid'Img
         & " of child  pid " & Child_Pid'Img);

    Event_Mng.Pause (3_000);
  end if;

exception
  when Sys_Calls.System_Error =>
    Basic_Proc.Put_Line_Error ("Exception System_Error raised");
    raise;
end T_Fork;

