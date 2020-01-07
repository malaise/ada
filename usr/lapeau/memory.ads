with Movements;
package Memory is

  ----------
  -- Game --
  ----------
  -- Initialise and save a new game
  subtype Req_Game_Range is Integer range -1 .. 999999;
  subtype Game_Range is Req_Game_Range range 0 .. Req_Game_Range'Last;
  Random_Num : constant Req_Game_Range := Req_Game_Range'First;
  procedure Start_Game (Num : in Req_Game_Range);

  -- Restore current game
  procedure Restore_Game;

  -------------------------
  -- Movements undo/redo --
  -------------------------
  -- Add a movement to list of undos
  -- Clears the list of redos
  procedure Add (Mov : in Movements.Movement);

  -- Pop a movement and add it to the list of redos
  function Can_Undo return Boolean;
  function Undo return Movements.Movement;

  -- Pop a undone movment and add it to the list of undos
  function Can_Redo return Boolean;
  function Redo return Movements.Movement;

  -- Clear the redoes
  procedure Clear_Redoes;

end Memory;

