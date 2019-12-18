with Movements;
package Memory is

  ----------
  -- Game --
  ----------
  -- Initialise and save a new game
  procedure Start_Game;

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

