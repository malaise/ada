with Rnd, Argument, Basic_Proc;
with Game, Debug;
procedure Land_Lem is
  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage : " & Argument.Get_Program_Name
        & " [ -H | --hard ] [ -g | --grab ] [ -p | --pause ]");
  end Help;

  -- Result of a game
  -- Trigger a new environment at startup (as if prev game succeeded)
  Result : Game.Result_List := Game.Landed;
  -- Start first game as paused
  Pause : Boolean := False;
  use type Game.Result_List;
begin
  Debug.Init;
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter(1) = "-h"
            or else Argument.Get_Parameter(1) = "--help")  then
    Help;
    return;
  end if;
  -- Check for option -p / --pause
  if Argument.Is_Set (1, "p")
  and then Argument.Get_Parameter (1, "p") = "" then
    -- "-p"
    Pause := True;
  elsif Argument.Is_Set (1, "-pause")
  and then Argument.Get_Parameter (1, "-pause") = "" then
    -- "--pause"
    Pause := True;
  end if;

  Rnd.Gen.Randomize;
  -- Do one game until end
  loop
    -- Start a new game if success, otherwise redo the same
    Result := Game.Play_One (Pause, Result = Game.Landed);
    exit when Result = Game.Aborted;
    Pause := False;
  end loop;
end Land_Lem;
