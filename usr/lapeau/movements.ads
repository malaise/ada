with Cards, Table;
package Movements is

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : in Cards.Card_Access) return Boolean;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : in Cards.Card_Access) return Boolean;

  -- Movement
  type Movement is record
     Card : Cards.Card_Access;
     From, To : Table.Stack_Range;
  end record;

  -- Do a movement
  procedure Move (Mov : Movement);

end Movements;

