with Sok_Types;
with Sok_Input;

-- Movement manager of Sokoban
package Sok_Movement is


  -- Man movement
  subtype Movement_List is Sok_Input.Key_List
   range Sok_Input.Left .. Sok_Input.Down;

  -- result of a movement try
  type Result_List is
   (Refused, Done, Box_Moved, Box_Ok_More, Box_Ok_Less);

  -- result of a movement to save
  subtype Saved_Result_List is Result_List range Done .. Box_Moved;

  -- what to save and give back to undo
  type Saved_Data_Rec is record
    -- position of man before movement
    Pos_Orig  : Sok_Types.Coordinate_Rec;
    -- direction of movement
    Movement  : Movement_List;
    -- result
    Result   : Saved_Result_List;
  end record;

  -- result of an undone movement
  subtype Undo_Result_List is Result_List range Done .. Box_Ok_Less;


  -- try to do a movement
  -- Give frame, current position and movement to try
  procedure Do_Movement (
   Frame    : in out Sok_Types.Frame_Tab;
   Position : in out Sok_Types.Coordinate_Rec;
   Movement : in Movement_List;
   Result   : out Result_List);


  -- to undo a movement
  -- give frame, current position and movement which
  -- moved to current position
  -- indicate also if a box was moved
  procedure Undo_Movement (
   Frame      : in out Sok_Types.Frame_Tab;
   Saved_Data : in Saved_Data_Rec;
   Result        : out Undo_Result_List;
   Prev_Position : out Sok_Types.Coordinate_Rec);


  -- raised on Do_Movement if movement is not coherent
  -- or     on Undo_Movement if it is not coherent
  Illegal_Movement : exception;

end Sok_Movement;

