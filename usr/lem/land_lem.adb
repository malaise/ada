with Game;
procedure Land_Lem is
begin
  -- Do one game until end
  loop
    exit when not Game.Play_One;
  end loop;
end Land_Lem;
