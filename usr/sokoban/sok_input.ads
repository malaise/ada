package Sok_Input is

  type Key_List is (Left, Right, Up, Down, Undo, Esc, Next, Refresh);

  function Get_Key return Key_List;

  procedure Pause;

  Break_Requested : exception;

  procedure End_Of_Program;

end Sok_Input;
