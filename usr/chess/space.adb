package body Space is

  function Opponent (Color : Color_List) return Color_List is
  begin
    if Color = White then
      return Black;
    else
      return White;
    end if;
  end Opponent;

  function Color_Of_Square (Square : Square_Coordinate) return Color_List is
  begin
    -- A1 (bottom left) is black
    if (Row_Range'Pos(Square.Row)
      + Col_Range'Pos(Square.Col)) mod 2 = 0 then
      return White;
    else
      return Black;
    end if;
  end Color_Of_Square;

  function Compute_Movement (Orig_Square : Square_Coordinate;
                             Col_Offset, Row_Offset : Movement_Range)
           return Movement_Result is
    Row : Row_Range;
    Col : Col_Range;
  begin
    Row := Row_Range'Val(Row_Range'Pos(Orig_Square.Row) + Integer(Row_Offset));
    Col := Col_Range'Val(Col_Range'Pos(Orig_Square.Col) + Integer(Col_Offset));
    return (Valid => True, Square => (Col, Row) );
  exception
    when others =>
      return (Valid => False);
  end Compute_Movement;
end Space;

