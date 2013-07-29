with Rnd, Argument, Basic_Proc;
with Game;
procedure Land_Lem is
  -- Result of a game
  -- Trigger a new environment at startup (as if prev game succeeded)
  Result : Game.Result_List := Game.Landed;
  use type Game.Result_List;
begin
  if Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter(1) = "--help" then
    Basic_Proc.Put_Line_Error ("Usage : " & Argument.Get_Program_Name
        & " [ -h | --hard ] [ -g | --grab ]");
    return;
  end if;
  Rnd.Gen.Randomize;
  -- Do one game until end
  loop
    Result := Game.Play_One (Result = Game.Landed);
    exit when Result = Game.Aborted;
  end loop;
end Land_Lem;
