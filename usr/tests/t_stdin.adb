with Ada.Exceptions, Ada.Characters.Latin_1;
with Argument, Event_Mng, Sys_Calls, Async_Stdin, Text_Line;

procedure T_Stdin is

  Arg_Error : exception;

  -- Fd and File to pipe
  Fd : Sys_Calls.File_Desc;
  File : Text_Line.File_Type;

  Go_On : Boolean;

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Async_Stdin.Put_Line_Out ("Aborted.");
    Sig := True;
  end Signal_Cb;

  function Stdin_Cb(Str : in String) return Boolean is
    Len : Natural := Str'Length;
    Last : Natural := Str'Last;
  begin
    if Len = 0 then
      Go_On := False;
      return True;
    end if;
    if Len >= 1 and then Str(Last) = Ada.Characters.Latin_1.Eot then
      Len := Len - 1;
      Last := Last - 1;
      Go_On := False;
    end if;
    File.Put (Str);
    File.Flush;
    return True;
  end Stdin_Cb;

  procedure Close is
  begin
    if File.Is_Open then
      File.Close;
    end if;
    Sys_Calls.Close (Fd);
  end Close;

begin

  -- Check argument
  if Argument.Get_Nbre_Arg /= 1 then
    raise Arg_Error;
  end if;
  begin
    Fd := Sys_Calls.Open (Argument.Get_Parameter, Sys_Calls.Out_File);
  exception
    when others =>
      Async_Stdin.Put_Line_Err ("Can't open pipe file "
                               & Argument.Get_Parameter);
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  File.Open (Text_Line.Out_File, Fd);

  -- Set async stdin
  begin
    Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, 21);
  exception
    when Async_Stdin.Error =>
      Async_Stdin.Put_Line_Out("Cannot set stdin async");
      Close;
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;

  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  -- Main loop
  Go_On := True;
  loop
    if Event_Mng.Wait(-1) then
      exit when not Go_On;
    end if;
    exit when Sig;
  end loop;

  Async_Stdin.Set_Async;
  Close;

exception
  when Arg_Error =>
    Async_Stdin.Put_Line_Err ("Usage: " & Argument.Get_Program_Name
                   & " <pipe_file>");
    Sys_Calls.Set_Error_Exit_Code;
  when Error : others =>
    Async_Stdin.Put_Line_Err ("Exception: " & Ada.Exceptions.Exception_Name(Error));
    Async_Stdin.Set_Async;
    Close;
    Sys_Calls.Set_Error_Exit_Code;
end T_Stdin;

