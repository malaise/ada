separate(Pieces)
procedure Actions_Of_Queen (Piece : in Queen_Piece) is
  use type Space.Movement_Range;
begin

  -- Row/Col
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, Offset, 0);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, -Offset, 0);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, 0, Offset);
  end loop;
  for Offset in 1 .. Space.Movement_Range'Last loop
    exit when not Add_Action(Piece, 0, -Offset);
  end loop;

  -- Diags
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

end Actions_Of_Queen;

