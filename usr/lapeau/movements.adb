with Table, Memory;
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
  function Can_Move (Source, Target : in Cards.Card_Access) return Boolean is
  begin
    if Source.Stack.Name = Cards.Deck.Symbol_Name then
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
      -- When undo from Done stack
      Curr.Movable := True;
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

  -- Can a card be purged
  -- When policy is Same_Suit => basic validity
  -- When Policy is Alternate_Color => Basic validity and not mode than
  --  one delta among colors in Done stacks
  function Can_Be_Purged (Acard : Cards.Card_Access) return Boolean is
    -- Tops of Done stack of alternate color
    Top_Done : Cards.Card_Access;
    Alternate_Suits : Cards.Suits_Pair;
    Alternate_Dones : array (1 .. 2) of Cards.Card_Access;
    Min : Integer;
  begin
    -- Top of target Done stack
    Top_Done := Cards.The_Dones(Acard.Suit)'Access;
    if Top_Done.Nb_Children /= 0 then
      Top_Done := Top_Done.Prev;
    end if;
    -- Basic validity versus top of corresponding Done stack
    if not Is_Valid (Acard, Top_Done) then
      return False;
    elsif Stack_Policy = Same_Suit then
      return True;
    end if;
    -- In Alternate_Colors, not more than one delta
    Alternate_Suits := Cards.Suits_Of (Cards.Alternate_Color (
        Cards.Color_Of (Acard.Suit)));
    Alternate_Dones(1) := Cards.The_Dones(Alternate_Suits(1))'Access;
    Alternate_Dones(2) := Cards.The_Dones(Alternate_Suits(2))'Access;
    -- Not more than a delta of 2
    Min := Integer (Acard.Name) - 2;
    return        Min < Alternate_Dones(1).Nb_Children
        and then  Min < Alternate_Dones(2).Nb_Children;
  end Can_Be_Purged;

  -- Move all possible cards into Done stacks
  procedure Purge is
    One_Moved : Boolean;
    Acc, Target : Cards.Card_Access;
    Mov : Movement;
    use type Cards.Card_Access;
  begin
    -- Loop until no move
    Iter:
    loop
      One_Moved := False;
      -- Loop for all stacks
      Stacks:
      for Stack in Cards.Stack_Range loop
         Acc := Cards.The_Stacks(Stack).Prev;
         -- Loop in the stack until no move
         Depth:
         loop
           exit when Acc = null or else Acc = Cards.The_Stacks(Stack)'Access;
           if Can_Be_Purged (Acc) then
             Target := Cards.The_Dones(Acc.Suit)'Access;
             if Target.Nb_Children /= 0 then
               Target := Target.Prev;
             end if;
             Mov := (Card => Acc, From => Acc.Stack, To => Target.Stack);
             Move (Mov);
             Memory.Add (Mov);
             One_Moved := True;
           else
             exit Depth;
           end if;
           Acc := Acc.Prev;
         end loop Depth;
      end loop Stacks;
      exit Iter when not One_Moved;
    end loop Iter;
  end Purge;

end Movements;

