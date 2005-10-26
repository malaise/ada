package Space is

  type Col_Range is (A, B, C, D, E, F, G, H);

  type Row_Range is new Positive range 1 .. 8;

  type Color_List is (White, Black);
  function Opponent (Color : Color_List) return Color_List;

  type Square_Coordinate is record
    Col : Col_Range;
    Row : Row_Range;
  end record;
  Origin : constant Square_Coordinate := (A, 1);

  type Square_Array is array (Positive range <>) of Square_Coordinate;

  function Color_Of_Square (Square : Square_Coordinate) return Color_List;

  -- Movement offset in rows or columns
  type Movement_Range is new Integer
                range -Integer(Row_Range'Last) .. Integer(Row_Range'Last);
  -- Result of movement computation
  type Movement_Result (Valid : Boolean := True) is record
    case Valid is
      when True =>
        Square : Square_Coordinate;
      when False =>
        null;
    end case;
  end record;

  function Compute_Movement (Orig_Square : Square_Coordinate;
                             Col_Offset, Row_Offset : Movement_Range)
           return Movement_Result;

end Space;

