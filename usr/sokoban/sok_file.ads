-- Frames reading, saving and restoring.
with Sok_Types;
package Sok_File is
  -- Internal state of a frame
  type State_Rec is record
    Frame        : Sok_Types.Frame_Tab;
    No_Frame     : Sok_Types.Frame_Range;
    Position     : Sok_Types.Coordinate_Rec;
    Nbre_Targets : Natural;
    Box_Ok       : Natural;
    Moves        : Natural;
    Pushes       : Natural;
    Score        : Sok_Types.Score_Rec;
  end record;

  -- Ensure that frames are readable
  -- Init empty score file if necessary
  procedure Init;

  -- To read a new frame
  procedure Read (No_Frame : in  Sok_Types.Frame_Range;
                  Frame    : out Sok_Types.Frame_Tab);
  Data_File_Not_Found, Error_Reading_Data : exception;

  -- Save a frame and recover a frame, with saved movements
  procedure Save (State : in State_Rec);
  Error_Writing_Frame : exception;

  procedure Restore (State : out State_Rec);
  Frame_File_Not_Found, Error_Reading_Frame : exception;

  -- Initialise scores, read/update/reset score
  function Read_Score (No : Sok_Types.Frame_Range;
                       Name : String := "") return Sok_Types.Score_Rec;
  procedure Write_Score (No : in Sok_Types.Frame_Range;
                         Score : in Sok_Types.Score_Rec);
  procedure Reset_Score (No : in Sok_Types.Frame_Range);
  Score_Io_Error : exception;

end Sok_File;

