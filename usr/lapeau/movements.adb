with Basic_Proc;
with Table, Memory;
package body Movements is

  -- Number of free stacks
  Nb_Free_Stacks : Natural range 0 .. Cards.Stack_Range'Last;

  procedure Reset is
  begin
     Nb_Free_Stacks := 0;
  end Reset;

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
    Nb_To_Move, Nb_Movable : Positive;
  begin
    if Source.Stack.Name = Cards.Deck.Symbol_Name then
      -- Cannot move from Done
      return False;
    end if;
    -- Must be valid,
    if not Is_Valid (Source, Target) then
      return False;
    end if;
    -- Nb children compatible with number of empty stacks
    Nb_To_Move := Source.Nb_Children + 1;
    Nb_Movable := Nb_Free_Stacks * (Nb_Free_Stacks + 1) / 2 + 1;
    return Nb_To_Move <= Nb_Movable;
  end Can_Move;

  --------------
  -- Movement --
  --------------
  -- Internal: Move one card
  procedure Move_One (Mov : Movement) is
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
      if Stack.Suit = Cards.Deck.Empty then
        Nb_Free_Stacks := Nb_Free_Stacks + 1;
      end if;
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
      -- The stack is empty
      Stack.Next := Curr;
      Curr.Prev := Stack;
      if Stack.Suit = Cards.Deck.Empty then
        Nb_Free_Stacks := Nb_Free_Stacks - 1;
      end if;
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
Basic_Proc.Put_Line_Error ("Moving " & Mov.Card.Image
  & " to " & Stack.Image);
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
    Table.Console.Flush;
  end Move_One;

  -- Internal data for multiple move
  Available_Stacks : array (Cards.Stack_Range) of Boolean;
  Nb_Available : Natural range 0 .. Cards.Stack_Range'Last;
  -- Internal: Find next available stack
  function Next_Available return Cards.Stack_Range is
  begin
    for Stack in Cards.Stack_Range loop
      if Available_Stacks(Stack) then
Basic_Proc.Put_Line_Error ("Next available => " & Cards.The_Stacks(Stack).Image);
        return Stack;
      end if;
    end loop;
    raise Program_Error;
  end Next_Available;
  -- Internal: Adjust Stack_Free after a move
  procedure Adjust_Available (Source, Target : Cards.Card_Access) is
  begin
    if Source.Nb_Children = 0 then
      -- Maybe opening a free stack
Basic_Proc.Put_Line_Error ("Adjust => " & Source.Image);
      Available_Stacks(Source.Name) := True;
      Nb_Available := Nb_Available + 1;
    end if;
    if Target.Nb_Children = 1 then
      Available_Stacks(Target.Name) := False;
      Nb_Available := Nb_Available - 1;
    end if;
  end Adjust_Available;

  -- Internal: Move several cards recursively
  procedure Move_Multiple (Mov : Movement) is
    Available : Cards.Stack_Range;
    Child :  Cards.Card_Access;
    use type Cards.Card_Access;
  begin
    if Mov.Card.Next = null then
      -- One card to move
      Move_One (Mov);
      Adjust_Available (Mov.From, Mov.To);
      return;
    end if;

    -- Move child to a free stack
Basic_Proc.Put_Line_Error ("Multi Moving " & Mov.Card.Image
  & " to " & (if Mov.To.Prev = null then Mov.To.Image else Mov.To.Prev.Image));
    Available := Next_Available;
    Available_Stacks(Available) := False;
    Child := Mov.Card.Next;
    Move_Multiple ( (Card => Mov.Card.Next,
                     From => Mov.From,
                     To   => Cards.The_Stacks(Available)'Access) );
    -- Move card (now top) to target
    Move_One (Mov);
    Adjust_Available (Mov.From, Mov.To);
    -- Move child to target
    Move_Multiple ( (Card => Child,
                     From => Cards.The_Stacks(Available)'Access,
                     To =>   Mov.To) );
    -- Move top to target

  end Move_Multiple;

  -- Move some cards
  procedure Move (Mov : Movement) is
    use type Cards.Card_Access;
  begin
    if Mov.Card.Next = null then
      -- One card to move
      Move_One (Mov);
      return;
    end if;
    -- Prepare for multiple move
    -- Identify the available stacks (excluding dest)
    Nb_Available := 0;
    for Stack in Cards.Stack_Range loop
      Available_Stacks(Stack) := Cards.The_Stacks(Stack).Nb_Children = 0
          and then Mov.To /= Cards.The_Stacks(Stack)'Access;
      if Available_Stacks(Stack) then
        Nb_Available := Nb_Available + 1;
      end if;
    end loop;
Basic_Proc.Put_Line_Error ("Start Multi Move");
    Move_Multiple (Mov);
Basic_Proc.Put_Line_Error ("End Multi Move");
  end Move;

  -----------
  -- Purge --
  -----------

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

