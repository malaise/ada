with Cards;
package Movements is

 -- Reset internal counters at game startup
  procedure Reset;

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : Cards.Card_Access) return Boolean;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : Cards.Card_Access) return Boolean;

  -- Movement
  type Movement is record
    -- The card to move
     Card : Cards.Card_Access;
     -- The stack pseudo card of source and target
     From, To : Cards.Card_Access;
     -- The card below From was face down
     From_On_Facedown : Boolean := False;
  end record;

  -- Do a movement
  procedure Move (Mov : Movement; Add : in Boolean);

  -- Move all possible cards into Done stacks
  procedure Purge;

  -- Move top of Reserve to top of Pull, face up
  -- If forward is False then undo a move to Pull
  procedure Move_To_Pull (Forward : in Boolean; Add : in Boolean);

  -- Refill the reserve stack with the (reverse) content of the pull stack
  -- May be allowed only twice per game
  Limit_Refill : Boolean := False;
  -- If forward is False then undo a refill
  -- If Add then push one movement from Pull to Reserve
  procedure Refill (Forward : in Boolean; Add : in Boolean);

end Movements;

