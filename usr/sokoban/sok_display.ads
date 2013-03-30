with Ada.Calendar;
with Con_Io;
with Sok_Types;
-- displaying of sokoban
package Sok_Display is

  -- to init screen
  procedure Init;
  function Get_Console return Con_Io.Console;

  -- puts all the frame
  procedure Put_Frame (Frame : in Sok_Types.Frame_Tab);

  -- puts a square
  procedure Put_Square (Square     : in Sok_Types.Square_Rec;
                        Coordinate : in Sok_Types.Coordinate_Rec;
                        Blink      : in Boolean := False);

  -- puts the down line
  procedure Put_Line (Moves : in Natural; Pushes : in Natural;
                      Boxes_In : in Natural; Nb_Boxes : in Positive;
                      Frame : in Sok_Types.Frame_Range);

  -- puts the score
  procedure Put_Score (Score : in Sok_Types.Score_Rec);

  -- puts the elapsed time
  procedure Put_Time (Day : in Natural; Time : in Ada.Calendar.Day_Duration);

  -- list of possible actions
  type Action_List is (Frame, Done, Write, Read, Reset, Get_New, Break);

  -- help window
  procedure Put_Help (Help : in Action_List);


  subtype Menu_Action_List is Action_List range Write .. Break;
  -- put extra menu with initial selection
  procedure Put_Menu (
   Init_Action : in Menu_Action_List;
   Allow_Write : in Boolean);
  procedure Clear_Menu;

  -- new action selected or mouse event
  procedure Update_Menu (New_Action : in Menu_Action_List;
                         Clicked : in Boolean);

  -- menu item corresponding to row and col (Done if none)
  subtype Got_Action_List is Action_List range Done .. Break;
  function Get_Action (Row, Col : Natural) return Got_Action_List;

  -- Errors
  type Error_List is (No_Data, Read, No_Frame, Restore, Save,
                      Format, Score_Io, Internal, Init_Score);
  procedure Put_Error (Error : in Error_List);
  procedure Clear_Error;

  -- clear screen
  procedure End_Of_Program;

  -- get frame number
  type Get_Result_List is (Set, Esc, Refresh);
  procedure Get_No_Frame (No : out Sok_Types.Frame_Range;
                          Result : out Get_Result_List);
  Format_Error : exception;

  -- ring a bell
  procedure Bell;

end Sok_Display;
