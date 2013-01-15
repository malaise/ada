-- Simple package to passively test if / wait until a key has been pressed
-- No echo of input
-- Not compatible with blocking inputs on stdin, of course.
with Ada.Characters.Latin_1;
package Key_Pressed is

  -- Error on Open, Close and Key_Pressed
  Error : exception;

  -- Prepare stdin for silent input, blocking or not
  -- Raises Error
  procedure Open (Blocking : in Boolean);

  -- Restore Stdin for synchronous input with echo
  -- Raises Error if Check and an error occurs
  procedure Close (Check : in Boolean := False);

  -- Check if a key has been pressed,
  --  return specific characters if not (only if open non blocking) or on error
  No_Key     : constant Character := Ada.Characters.Latin_1.Nul;
  Error_Key  : constant Character := Ada.Characters.Latin_1.Eot;
  function Get_Key return Character;

  -- Check if a key has been pressed or wait until a key is pressed,
  --  raise exception on error
  function Key_Pressed return Boolean;

end Key_Pressed;

