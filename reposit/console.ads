-- Basic consol managment
package Console is

  -- Clear screen
  procedure Clear;

  -- Move cursor left/right
  procedure Left;
  procedure Right;

  -- Set cursor at col N (From 1)
  procedure Set_Col (N : Positive);

  -- Delete cur char and shilft left the tail
  procedure Delete;

  -- Erase whole line
  procedure Erase_Line;

  -- Erase from beginning of line to cursor
  procedure Erase_Begin_Line;
  -- Erase from cursor to end of line
  procedure Erase_End_Line;

  -- Save/Restore cursor position
  procedure Save;
  procedure Restore;

  -- Switch reverse
  procedure Set_Reverse (On : in Boolean);

  -- Ring bell
  procedure Sound (N_Times : in Positive := 1);

end Console;

