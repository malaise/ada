with Trace.Loggers;
with Table, Memory;
package body Movements is

  -- Debug
  Logger : Trace.Loggers.Logger;

  -- Max number of refill per game
  Max_Refill : constant := 2;
  Nb_Refill : Natural;

  procedure Reset is
  begin
    Logger.Init ("Movements");
    Nb_Refill := 0;
  end Reset;

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : in Cards.Card_Access) return Boolean is
    use type Cards.Card_Access, Cards.Deck.Suit_List, Cards.Colors;
  begin
    if Source.Suit not in Cards.Deck.Suit_List then
      -- Not a real card (empty stack)
      return False;
    end if;
    if Cards.Is_Done_Stack (Target.Stack) then
      -- Done stack
      if Source.Suit /= Target.Suit then
        Logger.Log_Debug (
            "Is_Valid -> FALSE cause Done stack with different suit");
        return False;
      elsif Target.Stack.Nb_Children = 0 then
        -- Target is an empty done stack
        Logger.Log_Debug ("Is_Valid -> "
            & Boolean'Image (Source.Name = 1)
            & " on empty Done stack");
        return Source.Name = 1;
      else
        Logger.Log_Debug ("Is_Valid -> "
            & Boolean'Image (Source.Name = Target.Name + 1)
            & " on Done stack");
        return Source.Name = Target.Name + 1;
      end if;
    elsif Cards.Is_Play_Stack (Target.Stack) then
      -- Target is in a play stack
      if Target = Target.Stack then
        -- Target is a play stack, it must be empty
        Logger.Log_Debug ("Is_Valid -> "
           & Boolean'Image (Target.Nb_Children = 0 and then
                            Source.Name = Cards.Deck.Name_Range'Last)
           & " on empty play stack");
        return Target.Nb_Children = 0
               and then Source.Name = Cards.Deck.Name_Range'Last;
      end if;
      if Source.Name /= Target.Name - 1 then
        -- Target is a card of a Play stack
        Logger.Log_Debug ("Is_Valid -> FALSE cause not crescent");
        return False;
      end if;
      -- Alternate color
      Logger.Log_Debug ("Is_Valid -> "
          & Boolean'Image (Cards.Color_Of (Source.Suit)
                        /= Cards.Color_Of (Target.Suit))
          & " on compatible colors");
      return Cards.Color_Of (Source.Suit) /= Cards.Color_Of (Target.Suit);
    else
      Logger.Log_Debug ("Is_Valid -> FALSE cause target is in Dock");
      return False;
    end if;
  end Is_Valid;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : in Cards.Card_Access) return Boolean is
    use type Cards.Card_Access;
  begin
    -- Must be valid,
    if not Is_Valid (Source, Target) then
      Logger.Log_Debug ("Can_Move -> FALSE cause not valid");
      return False;
    end if;

    if Target.Stack.Name = Cards.Deck.Symbol_Name then
      -- Cards go to Done one by one
      Logger.Log_Debug ("Can_Move -> "
          & Boolean'Image (Source.Nb_Children = 0)
          & " on several cards to Done stack");
      return Source.Nb_Children = 0;
    end if;

    if Target.Next /= null then
      -- Target is not top of stack (or stack but not empty???)
      Logger.Log_Debug ("Can_Move -> FALSE cause target not on top of stack");
      return False;
    end if;
    return True;
  end Can_Move;

  --------------
  -- Movement --
  --------------
  -- Internal: Move one card
  procedure Move_One (Mov : Movement; Add : in Boolean; Wait : in Boolean) is
    Curr, Prev, Stack : Cards.Card_Access;
    Nb_Children : Natural;
    Pushed : Movement;
    use type Cards.Card_Access, Cards.Deck.Full_Suit_List;
  begin
    Logger.Log_Debug ("Moving " & Mov.Card.Image
                      & " and " & Mov.Card.Nb_Children'Img & " children"
                      & " from " & Mov.From.Image & " to " & Mov.To.Image);

    -- Adjust the stack From
    Curr := Mov.Card;
    Prev := Mov.Card.Prev;
    Stack := Mov.From;
    if Stack.Next = Mov.Card then
      -- The stack becomes empty
      Stack.Next := null;
      Stack.Prev := null;
    else
      -- Prev becomes top of stack
      Prev.Next := null;
      Prev.Nb_Children := 0;
      Prev.Movable := True;
      Stack.Prev := Prev;
      -- Update parents in source stack
      if Stack.Suit = Cards.Deck.Empty and then Cards.Is_Play_Stack (Stack) then
        Nb_Children := 0;
        Curr := Curr.Prev;
        Prev := null;
        while Curr /= Stack loop
          -- Top of stack (not yet face up) or any sequence face up is movable
          Curr.Movable := Prev = null
          or else (Curr.Xcard.Is_Face_Up
                   and then Is_Valid (Prev, Curr) );
          if Curr.Movable then
            Curr.Nb_Children := Nb_Children;
            Nb_Children := Nb_Children + 1;
          else
            Curr.Nb_Children := 0;
            Nb_Children := 0;
          end if;
          Prev := Curr;
          Curr := Curr.Prev;
        end loop;
      end if;
    end if;
    -- Move Card and its children
    Stack.Nb_Children := Stack.Nb_Children - Mov.Card.Nb_Children - 1;

    -- Adjust the stack To
    Stack := Mov.To;
    Prev := Mov.Card.Prev;
    Nb_Children := Stack.Nb_Children;
    -- Link to top of stack to new parent
    Curr := Mov.Card;
    if Stack.Nb_Children = 0 then
      -- The stack is empty
      Stack.Next := Curr;
      Curr.Prev := Stack;
    else
      if Mov.From_On_Facedown then
        -- Undo moves back on a card that was face down
        Stack.Prev.Xcard.Turn_Over (False);
        Stack.Prev.Xcard.Show (True);
        Stack.Prev.Xcard.Do_Raise;
      end if;
      Stack.Prev.Next := Curr;
      Curr.Prev := Stack.Prev;
    end if;
    -- Moved cards change stack
    Curr := Mov.Card;
    loop
      Curr.Stack := Stack;
      exit when Curr.Next = null;
      Curr := Curr.Next;
    end loop;
    Stack.Prev := Curr;
    Stack.Nb_Children := Stack.Nb_Children + Mov.Card.Nb_Children + 1;
    -- Update parents
    Curr := Mov.Card;
    if not Cards.Is_Done_Stack (Stack) then
      -- When undo from Done stack
      Curr.Movable := True;
      -- Increment the number of children of each ancestor of a play stack
      if Cards.Is_Play_Stack (Stack) then
        loop
          Curr := Curr.Prev;
          exit when not Curr.Movable;
          Curr.Nb_Children := Curr.Nb_Children + Mov.Card.Nb_Children + 1;
        end loop;
      end if;
    end if;

    -- Move the X cards
    Curr := Mov.Card;
    loop
      if Cards.Is_Done_Stack (Stack) then
        Curr.Xcard.Move (Table.Done_Of (Suit => Stack.Suit) );
      elsif Cards.Is_Play_Stack (Stack) then
        Curr.Xcard.Move (Table.Play_Of (
            Play  => Stack.Name,
            Depth => Nb_Children + 1) );
      elsif Stack = Cards.The_Dock (Cards.Dock_Pull)'Access then
        -- Undo a move from pull (to play or Done)
        Curr.Xcard.Move (Table.Dock_Of (Cards.Dock_Pull));
      else
        -- Undo a move from reserve to pull
        Curr.Xcard.Move (Table.Dock_Of (Cards.Dock_Reserve));
      end if;
      Curr.Xcard.Show (True);
      Curr.Xcard.Do_Raise;
      exit when Curr.Next = null;
      -- Wait if several cards moved
      Table.Console.Flush;
      Table.Wait (0.1);
      Curr := Curr.Next;
      Nb_Children := Nb_Children + 1;
    end loop;

    -- Save and show
    Pushed := Mov;
    if not Cards.Is_Done_Stack (Prev.Stack)
    and then not Prev.Xcard.Is_Face_Up then
      Pushed.From_On_Facedown := True;
      Prev.Xcard.Turn_Over (True);
      Prev.Movable := True;
      Logger.Log_Debug ("Revealing " & Prev.Image);
    end if;
    if Add then
      Memory.Add (Pushed);
    end if;
    Prev.Xcard.Show (True);
    Prev.Xcard.Do_Raise;
    if Wait then
      Table.Console.Flush;
      Table.Wait (0.1);
    end if;
  end Move_One;

  -- Move some cards
  procedure Move (Mov : Movement; Add : in Boolean) is
  begin
    Move_One (Mov, Add, False);
  end Move;

  -----------
  -- Purge --
  -----------

  -- Can a card be purged
  --  one delta among colors in Done stacks
  function Can_Be_Purged (Acard : Cards.Card_Access) return Boolean is
    -- Tops of Done stack of alternate color
    Top_Done : Cards.Card_Access;
    Alternate_Suits : Cards.Suits_Pair;
    Alternate_Dones : array (1 .. 2) of Cards.Card_Access;
    Min : Integer;
  begin
    -- Top of target Done stack
    Top_Done := Cards.The_Done(Acard.Suit)'Access;
    if Top_Done.Nb_Children /= 0 then
      Top_Done := Top_Done.Prev;
    end if;
    -- Basic validity versus top of corresponding Done stack
    if not Is_Valid (Acard, Top_Done) then
      return False;
    end if;
    -- In Alternate_Colors, not more than one delta
    Alternate_Suits := Cards.Suits_Of (Cards.Alternate_Color (
        Cards.Color_Of (Acard.Suit)));
    Alternate_Dones(1) := Cards.The_Done(Alternate_Suits(1))'Access;
    Alternate_Dones(2) := Cards.The_Done(Alternate_Suits(2))'Access;
    -- Not more than a delta of 2
    Min := Acard.Name - 2;
    return        Min <= Alternate_Dones(1).Nb_Children
        and then  Min <= Alternate_Dones(2).Nb_Children;
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
      Play_Stacks:
      for Stack in Cards.Play_Stack_Range loop
        Acc := Cards.The_Play(Stack).Prev;
        -- Loop in the stack until no move
        Depth:
        while Acc /= null and then Acc /= Cards.The_Play(Stack)'Access loop
          if Can_Be_Purged (Acc) then
            Target := Cards.The_Done(Acc.Suit)'Access;
            if Target.Nb_Children /= 0 then
              Target := Target.Prev;
            end if;
            Mov := (Card => Acc, From => Acc.Stack, To => Target.Stack,
                    others => <>);
            Move_One (Mov, True, True);
            One_Moved := True;
          else
            exit Depth;
          end if;
          Acc := Acc.Prev;
        end loop Depth;
      end loop Play_Stacks;
      exit Iter when not One_Moved;
    end loop Iter;
  end Purge;

  -- Move top of Reserve to top of Pull, face up
  procedure Move_To_Pull (Forward : in Boolean; Add : in Boolean) is
    Source, Target, Card : Cards.Card_Access;
    use type Cards.Card_Access;
  begin
    if Forward then
      -- Move the card from Reserve to Pull
      Source := Cards.The_Dock(Cards.Dock_Reserve)'Access;
      Target := Cards.The_Dock(Cards.Dock_Pull)'Access;
    else
      Source := Cards.The_Dock(Cards.Dock_Pull)'Access;
      Target := Cards.The_Dock(Cards.Dock_Reserve)'Access;
    end if;
    Card := Source.Prev;
    -- Unlink card from Source
    if Source.Next = Card then
      -- Last card of Source
      Source.Next := null;
      Source.Prev := null;
    else
      Source.Prev := Card.Prev;
      Source.Prev.Next := null;
      Source.Prev.Movable := True;
      Source.Prev.Xcard.Do_Raise;
      Source.Prev.Xcard.Show (True);
    end if;
    -- Link Target to card
    if Target.Next = null then
      -- First card in target
      Target.Next := Card;
      Card.Prev := Target;
    else
      Target.Prev.Next := Card;
      Card.Prev := Target.Prev;
    end if;
    Target.Prev := Card;
    -- Update card
    Card.Stack := Target;
    Card.Next := null;
    Card.Movable := True;
    if Forward then
      Card.Xcard.Move (Table.Dock_Of (Cards.Dock_Pull));
      Card.Xcard.Turn_Over (Face_Up => True);
    else
      Card.Xcard.Move (Table.Dock_Of (Cards.Dock_Reserve));
      Card.Xcard.Turn_Over (Face_Up => False);
    end if;
    Card.Xcard.Show (True);
    Card.Xcard.Do_Raise;
    -- Last adjustments
    Source.Nb_Children := Source.Nb_Children - 1;
    Target.Nb_Children := Target.Nb_Children + 1;
    if Add then
      Memory.Add ((Card => Card,
                   From => Source,
                   To => Target,
                   others => <>));
    end if;
  end Move_To_Pull;

  -- Refill the reserve stack with the (reverse) content of the pull stack
  -- Allowed only twice per game
  -- If forward is False then undo a refill
  -- If Add then push one movement from Pull to Reserve
  procedure Refill (Forward : in Boolean; Add : in Boolean) is
    Source, Target, Card : Cards.Card_Access;
    use type Cards.Card_Access;
  begin
    -- Max refill
    if Forward and then Nb_Refill = Max_Refill then
       return;
    end if;
    if Forward then
      Nb_Refill := Nb_Refill + 1;
    else
      Nb_Refill := Nb_Refill - 1;
    end if;

    if Forward then
      -- Move the cards from Pull to Reserve
      Source := Cards.The_Dock(Cards.Dock_Pull)'Access;
      Target := Cards.The_Dock(Cards.Dock_Reserve)'Access;
    else
      -- Move the cards from Reserve to Pull
      Source := Cards.The_Dock(Cards.Dock_Reserve)'Access;
      Target := Cards.The_Dock(Cards.Dock_Pull)'Access;
    end if;
    while Source.Prev /= null loop
      Card := Source.Prev;
      -- Unlink card from Pull
      if Source.Next = Card then
        -- Last card of Pull
        Source.Next := null;
        Source.Prev := null;
      else
        Source.Prev := Card.Prev;
      end if;
      -- Link Reserve to card
      Card.Prev := Target.Prev;
      if Target.Next = null then
        Target.Next := Card;
      else
        Card.Prev.Next := Card;
      end if;
      Target.Prev := Card;
      -- Update card
      Card.Stack := Target;
      if Forward then
        Card.Xcard.Move (Table.Dock_Of (Cards.Dock_Reserve));
        Card.Xcard.Turn_Over (Face_Up => False);
      else
        Card.Xcard.Move (Table.Dock_Of (Cards.Dock_Pull));
        Card.Xcard.Turn_Over (Face_Up => True);
      end if;
      Card.Xcard.Show (False);
    end loop;
    -- Top of Reserve
    Card.Next := null;
    Card.Movable := True;
    -- Last adjustments
    Target.Nb_Children := Source.Nb_Children;
    Source.Nb_Children := 0;
    -- Show
    Source.Xcard.Do_Raise;
    Source.Xcard.Show (True);
    Target.Prev.Xcard.Do_Raise;
    Target.Prev.Xcard.Show (True);
    if Add then
      Memory.Add ((From => Cards.The_Dock(Cards.Dock_Pull)'Access,
                   To => Cards.The_Dock(Cards.Dock_Reserve)'Access,
                   others => <>));
    end if;

  end Refill;

end Movements;

