-- Simple package to passively test if / wait until a key has been pressed
-- No echo of input
-- Not compatible with blocking inputs on stdin, of course.
with Aski;
package Key_Pressed is

  -- Error on Open, Close and Key_Pressed
  Error : exception;

  -- Prepare stdin for silent input
  -- Raises Error in case of error
  procedure Open;

  -- Restore Stdin for synchronous input with echo
  -- Raises Error if an error occurs and Check is set
  procedure Close (Check : in Boolean := False);

  -- Check if a key has been pressed
  -- If yes then return it
  -- Otherwise
  --   If not Blocking then return No_Key,
  --   Otherwise wait until a key is pressed
  -- Return Error_Key on error
  No_Key     : Character renames Aski.Nul;
  Error_Key  : Character renames Aski.Eot;
  function Get_Key (Blocking : Boolean := False) return Character;

  -- If open non blocking then check if a key has been pressed,
  -- Otherwise wait until a key is pressed and return True
  -- Raise exception Error on error
  function Key_Pressed (Blocking : Boolean := False) return Boolean;

end Key_Pressed;

