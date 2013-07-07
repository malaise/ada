-- Common types for sokoban
package Sok_Types is

  -- Array of a frame
  subtype Row_Range is Positive range 1 .. 16;
  subtype Col_Range is Positive range 1 .. 19;

  -- Fixed background of the square
  type Pattern_List is (Wall, Free, Target);
  -- changeable content of a square
  type Content_List is (Man, Box, Nothing);

  -- A square
  type Square_Rec (Pattern : Pattern_List := Free) is record
    case Pattern is
      -- nothing on a wall
      when Wall => null;
      -- man or box or nothing
      when Free | Target =>
        Content : Content_List;
    end case;
  end record;

  -- A frame
  type Frame_Tab is array (Row_Range, Col_Range) of Square_Rec;

  -- Frame number (0 for restaure)
  subtype Desired_Frame_Range is Natural range 0 .. 50;
  Restore_Frame : constant Desired_Frame_Range := 0;
  subtype Frame_Range is Desired_Frame_Range
                         range 1 .. Desired_Frame_Range'Last;

  -- Square position in frame
  type Coordinate_Rec is record
    Row : Row_Range;
    Col : Col_Range;
  end record;


  -- Best score of each frame
  type Score_Rec is record
    Set : Boolean;
    Day : Natural;
    Dur : Duration;
    Moves : Natural;
    Pushes : Natural;
  end record;
end Sok_Types;

