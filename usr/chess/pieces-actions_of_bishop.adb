separate(Pieces)
procedure Actions_Of_Bishop (Piece : in Bishop_Piece) is
  use type Space.Movement_Range;
begin

  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, Offset, Offset);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, -Offset, -Offset);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, Offset, -Offset);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, -Offset, Offset);
  end loop;

end Actions_Of_Bishop;

