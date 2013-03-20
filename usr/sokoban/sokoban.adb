-- Sokoban game: push boxes and move them to destination
with Basic_Proc, Argument, Rnd, Upper_Str;
with Sok_Types, Sok_Manager;

procedure Sokoban is
  No_Frame : Sok_Types.Desired_Frame_Range;
  Ok : Boolean := False;

  function Frame_Random is new Rnd.Discr_Random(Sok_Types.Frame_Range);

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                             & " [ <frame_number> | rnd | rst]");
    Basic_Proc.Put_Line_Error (" Frames are from 1 to 50");
  end Usage;

begin
  Rnd.Gen.Randomize;

  begin
    if Argument.Get_Nbre_Arg = 0 then
      No_Frame := Sok_Types.Frame_Range'First;
    elsif Argument.Get_Nbre_Arg = 1 then
      if Upper_Str (Argument.Get_Parameter) = "RND" then
        No_Frame := Frame_Random (Rnd.Gen.all);
      elsif Upper_Str (Argument.Get_Parameter) = "RST" then
        No_Frame := Sok_Types.Restore_Frame;
      else
        No_Frame := Sok_Types.Frame_Range'Value (Argument.Get_Parameter);
      end if;
    else
      raise Constraint_Error;
    end if;
    Ok := True;
  exception
    when others =>
      Usage;
      Ok := False;
  end;
  if Ok then
    Sok_Manager.Play_Game(First_Frame => No_Frame);
  end if;
exception
  when others =>
    delay 2.0;
    raise;
end Sokoban;

