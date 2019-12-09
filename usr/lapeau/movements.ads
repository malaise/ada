with Cards;
package Movements is

 -- Reset internal counters at game startup
  procedure Reset;

  -- Play stack validity: Either same suit or alternate color
  type Stack_Policy_List is (Same_Suit, Alternate_Color);
  Stack_Policy : Stack_Policy_List := Same_Suit;

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : Cards.Card_Access) return Boolean;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : Cards.Card_Access) return Boolean;

  -- Movement
  type Movement is record
     Card : Cards.Card_Access;
     -- The stack pseudo card
     From, To : Cards.Card_Access;
  end record;

  -- Do a movement
  procedure Move (Mov : Movement);

  -- Move all possible cards into Done stacks
  procedure Purge;

end Movements;

