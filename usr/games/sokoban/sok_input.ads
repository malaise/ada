with Con_Io;
package Sok_Input is

  -- Get a key (in fact an event), movements first
  type Key_List is (Left, Right, Up, Down, Undo, Esc, Next, Mouse, Refresh);
  subtype Movement_List is Key_List range Left .. Down;
  function Get_Key return Key_List;

  -- Last mouse event, click or release
  type Mouse_Event_Rec (Valid : Boolean := False) is record
    case Valid is
      when True =>
        Click : Boolean;
        Row : Con_Io.Row_Range;
        Col : Con_Io.Col_Range;
      when False =>
        null;
    end case;
  end record;
  function Get_Mouse return Mouse_Event_Rec;

  procedure Pause;

  -- Raised by Get_Key or pause if Ctrl Break
  Break_Requested : exception;

  -- Close
  procedure End_Of_Program;

end Sok_Input;
