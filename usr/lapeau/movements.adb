package body Movements is

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : in Cards.Card_Access) return Boolean is
    use type Cards.Deck.Suit_List;
  begin
    if Target.Xcard.Is_Empty then
      -- Target is an empty stack
      return True;
    elsif Source.Name = 1 then
      -- Ace can anly be put on empty stack
      return False;
    elsif Source.Suit /= Target.Suit then
      return False;
    elsif Target.Name = 1 then
      -- King on Ace
      return Source.Name = 13;
    else
      return Source.Name = Target.Name - 1;
    end if;
  end Is_Valid;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : in Cards.Card_Access) return Boolean is
  begin
    -- @@@
    -- Must be valid, and Nb children compatible with numùber of empty stacks
    return False;
  end Can_Move;

end Movements;

