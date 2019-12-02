with Cards;
package Movements is

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : Cards.Card_Access) return Boolean;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : Cards.Card_Access;
                     As_Undo : Boolean) return Boolean;

  -- Movement
  type Movement is record
     Card : Cards.Card_Access;
     -- The stack pseudo card
     From, To : Cards.Card_Access;
  end record;

  -- Do a movement
  procedure Move (Mov : Movement);

end Movements;

