separate(Pieces)
procedure Actions_Of_Knight (Piece : in Knight_Piece) is
  Dummy : Boolean;
  pragma Unreferenced (Dummy);
begin

  -- Any combination of (+/- 1 row, +/- 2 col) , (+/- 2 row, +/- 1 col)
  for Col in -2 .. 2 loop
    for Row in -2 .. 2 loop
      if abs (Col * Row) = 2 then
        Dummy := Add_Action (Piece,
             Space.Movement_Range(Row),
             Space.Movement_Range(Col) );
      end if;
    end loop;
  end loop;

end Actions_Of_Knight;

