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
    -- Must be valid,
    if not Is_Valid (Source, Target) then
      return False;
    end if;
    -- and Nb children compatible with numùber of empty stacks
    -- @@@
    return Source.Nb_Children = 0;
  end Can_Move;

  --------------
  -- Movement --
  --------------
  procedure Move (Mov : Movement) is
    Acc, Stack : Cards.Card_Access;
    use type Cards.Card_Access, Cards.Deck.Full_Suit_List;
  begin
    -- Adjust the stack and top card of From
    Acc := Mov.Card.Prev;
    if Acc.Suit = Cards.Deck.Empty then
      -- Parent was the Stack and becomes empty
      Acc.Prev := null;
      Acc.Next := null;
      Acc.Nb_Children := 0;
    else
      -- Parent becomes top of stack
      Acc.Next := null;
      Acc.Nb_Children := 0;
      Acc.Movable := True;
      -- Stack
      Stack := Acc.Stack;
      Stack.Prev := Acc;
      Stack.Nb_Children := Stack.Nb_Children - 1;
    end if;
    -- Link to top of stack of To and to new parent
    Acc := Mov.Card;
    Stack := Cards.The_Stacks (Mov.To)'Access;
    Acc.Stack := Stack;
    if Stack.Next = null then
      -- Stack was empty
      Acc.Prev := Stack;
    else
      Acc.Prev := Stack.Prev;
    end if;
    Stack.Nb_Children := Stack.Nb_Children + 1;
    Stack.Prev := Acc;
    Acc.Prev.Next := Acc;
    -- Increment the number of children of each ancestor
    loop
      Acc := Acc.Prev;
      exit when not Acc.Movable;
      Acc.Nb_Children := Acc.Nb_Children + 1;
    end loop;

    -- Move the X card
    Mov.Card.Xcard.Move (Table.Pos_Of (
        Stack => Mov.To,
        Depth => Cards.The_Stacks (Mov.To).Nb_Children) );
    Mov.Card.Xcard.Show (True);
  end Move;

end Movements;

