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

  -- Save/Restore cursor position
  procedure Save;
  procedure Restore;

  -- Switch reverse
  procedure Set_Reverse (On : in Boolean);

end Console;

