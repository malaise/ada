with Cards, Table;
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
  type Movement is record
     Card : Cards.Card_Access;
     From, To : Table.Stack_Range;
  end record;
  -- The lists are clear when a game starts
  -- Add a movement to list of undos
  -- Clears the list of redos
  procedure Add (Mov : in Movement);

  -- Pop a movement and add it to the list of redos
  function Undo return Movement;

  -- Pop a undone movment and add it to the list of undos
  function Redo return Movement;

end Memory;

