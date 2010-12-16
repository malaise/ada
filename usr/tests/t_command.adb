with As.U; use As.U;
with Argument, Many_Strings, Basic_Proc, Command;
procedure T_Command is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
     & " [ -s ] <command> [ { <argument> } ]");
  end Usage;

  Narg : Positive;
  Use_Sh : Boolean;
  Cmd : Asu_Us;

  Stdout : aliased Command.Flow_Rec(Command.List);
  Stderr : aliased Command.Flow_Rec(Command.Str);
  Exit_Code : Command.Exit_Code_Range;

  Str : Asu_Us;
begin

  -- Parse args
  if Argument.Get_Nbre_Arg = 0 then
    Usage;
    return;
  end if;
  if Argument.Get_Parameter (1) = "-s" then
    Narg := 2;
    Use_Sh := True;
  else
    Narg := 1;
    Use_Sh := False;
  end if;
  if Argument.Get_Nbre_Arg < Narg then
    Usage;
    return;
  end if;
  Argument.Get_Parameter (Cmd, Occurence => Narg);
  for I in Narg + 1 .. Argument.Get_Nbre_Arg loop
    Many_Strings.Cat (Cmd, Argument.Get_Parameter (I));
  end loop;

  -- Execute
  Command.Execute (Asu_Ts (Cmd), Use_Sh, Command.Both,
                   Stdout'Unrestricted_Access,
                   Stderr'Unrestricted_Access,
                   Exit_Code);
  if Exit_Code = -1 then
    Basic_Proc.Put_Line_Output ("Exit code: " & Exit_Code'Img);
    return;
  end if;

  -- Put output and Error flows
  Basic_Proc.Put_Line_Output ("Output flow:");
  if not Stdout.List.Is_Empty then
    Stdout.List.Rewind;
    loop
      Stdout.List.Get (Str);
      Basic_Proc.Put_Line_Output (">" & Asu_Ts (Str) & "<");
      exit when Stdout.List.Is_Empty;
    end loop;
  end if;

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Error flow:");
  Basic_Proc.Put_Line_Output (">" & Asu_Ts (Stderr.Str) & "<");

  Basic_Proc.New_Line_Output;
  Basic_Proc.Put_Line_Output ("Exit code: " & Exit_Code'Img);
end T_Command;

