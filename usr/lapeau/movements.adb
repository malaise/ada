with Table;
package body Movements is

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : in Cards.Card_Access) return Boolean is
    use type Cards.Deck.Suit_List, Cards.Colors;
  begin
    if Target.Stack.Suit = Cards.Deck.Empty then
      -- Play stack
      if Target.Stack.Nb_Children = 0 then
        -- Target is an empty stack
        return True;
      elsif Target.Nb_Children /= 0 then
        -- Target is not top of stack
        return False;
      elsif Source.Name /= Target.Name - 1 then
        return False;
      end if;
      case Stack_Policy is
        when Same_Suit =>
          return Source.Suit = Target.Suit;
        when Alternate_Color =>
          return Cards.Color_Of (Source.Suit) /= Cards.Color_Of (Target.Suit);
      end case;
    else
      -- Done stack
      if Source.Suit /= Target.Suit then
        return False;
      elsif Target.Stack.Nb_Children = 0 then
        -- Target is an empty done stack
        return Source.Name = 1;
      else
        return Source.Name = Target.Name + 1;
      end if;
    end if;
  end Is_Valid;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : in Cards.Card_Access;
                     As_Undo : Boolean) return Boolean is
  begin
    if not As_Undo and then Source.Stack.Name = Cards.Deck.Symbol_Name then
      -- Cannot move from Done
      return False;
    end if;
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
    Curr, Prev, Stack : Cards.Card_Access;
    use type Cards.Deck.Full_Suit_List;
  begin
    -- Adjust the stack and top card of From
    Curr := Mov.Card;
    Prev := Mov.Card.Prev;
    Stack := Mov.From;
    if Stack.Nb_Children = 1 then
      -- The stack becomes empty
      Stack.Next := null;
      Stack.Prev := null;
    else
      Prev.Next := null;
      Prev.Nb_Children := 0;
      Prev.Movable := True;
      Stack.Prev := Prev;
    end if;
    Stack.Nb_Children := Stack.Nb_Children - 1;

    -- Link to top of stack of To and to new parent
    Stack := Mov.To;
    Curr.Stack := Stack;
    if Stack.Nb_Children = 0 then
      -- The stack empty
      Stack.Next := Curr;
      Curr.Prev := Stack;
    else
      Stack.Prev.Next := Curr;
      Curr.Prev := Stack.Prev;
    end if;
    Stack.Prev := Curr;
    Stack.Nb_Children := Stack.Nb_Children + 1;

    if Stack.Suit = Cards.Deck.Empty then
      -- Increment the number of children of each ancestor of a play stack
      loop
        Curr := Curr.Prev;
        exit when not Curr.Movable;
        Curr.Nb_Children := Curr.Nb_Children + 1;
      end loop;
    else
      -- Done stack => Irreversible
      Curr.Movable := False;
    end if;

    -- Move the X card
    if Stack.Suit = Cards.Deck.Empty then
      Mov.Card.Xcard.Move (Table.Stack_Of (
          Stack => Stack.Name,
          Depth => Stack.Nb_Children) );
    else
      Mov.Card.Xcard.Move (Table.Done_Of (Suit => Stack.Suit) );
    end if;
    Prev.Xcard.Show (True);
    Prev.Xcard.Do_Raise;
    Mov.Card.Xcard.Do_Raise;

  end Move;

end Movements;

