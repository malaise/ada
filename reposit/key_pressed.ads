-- Simple package to passively test if / wait until a key has been pressed
-- No echo of input
-- Not compatible with blocking inputs on stdin, of course.
with Ada.Characters.Latin_1;
package Key_Pressed is

  -- Error on Open, Close and Key_Pressed
  Error : exception;

  -- Prepare stdin for silent input, blocking or not
  -- Raises Error in case of error
  procedure Open (Blocking : in Boolean);

  -- Restore Stdin for synchronous input with echo
  -- Raises Error an error occurs and Check is set
  procedure Close (Check : in Boolean := False);

  -- Check if a key has been pressed
  -- If yes then return it
  -- Otherwise
  --   If open non blocking then return No_Key,
  --   Otherwise wait until a key is pressed
  -- Return Error_Key on error
  No_Key     : constant Character := Ada.Characters.Latin_1.Nul;
  Error_Key  : constant Character := Ada.Characters.Latin_1.Eot;
  function Get_Key return Character;

  -- If open non blocking then check if a key has been pressed,
  -- Otherwise wait until a key is pressed and return True
  -- Raise exception Error on error
  function Key_Pressed return Boolean;

end Key_Pressed;

