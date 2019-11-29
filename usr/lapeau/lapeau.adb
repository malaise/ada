with Table, Memory;
procedure Lapeau is
  use type Table.Action_List;
begin
  -- Gmobal init
  Table.Init;

  -- Init game
  Memory.Start_Game;
  while Table.Wait_Event /= Table.Quit loop
    null;
  end loop;
end Lapeau;
