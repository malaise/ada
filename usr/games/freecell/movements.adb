with Queues, Trace.Loggers, Images, Long_Longs, As.U;
with Table, Memory;
package body Movements is

  -- Debug
  Logger : Trace.Loggers.Logger;

  -- Allow move back from Done stack
  Allow_From_Done : constant Boolean := False;

  -- Number of free play and tmp stacks
  Nb_Free_Play_Stacks : Natural range 0 .. Cards.Play_Stack_Range'Last;
  Nb_Free_Tmp_Stacks : Natural range 0 .. Cards.Tmp_Stack_Range'Last;

  procedure Reset is
  begin
    Logger.Init ("Movements");
    Nb_Free_Play_Stacks := 0;
    Nb_Free_Tmp_Stacks := Cards.Tmp_Stack_Range'Last;
  end Reset;

  -- Can source card be put on target, basic card/card validity
  function Is_Valid (Source, Target : in Cards.Card_Access) return Boolean is
    use type Cards.Card_Access, Cards.Deck.Suit_List, Cards.Colors;
  begin
    if Source.Suit not in Cards.Deck.Suit_List then
      -- Not a real card
      return False;
    end if;
    if not Cards.Is_Done_Stack (Target.Stack) then
      -- Target is in a play stack or a tmp stack
      if Target = Target.Stack then
        -- Target is a play or tmp stack, it must be empty
        Logger.Log_Debug ("Is_Valid -> "
           & Boolean'Image (Target.Nb_Children = 0)
           & " on empty play stack");
        return Target.Nb_Children = 0;
      end if;
      if not Cards.Is_Play_Stack (Target.Stack) then
        -- Target is a card in a Tmp stack
        Logger.Log_Debug ("Is_Valid -> FALSE cause used tmp stack");
        return False;
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
    end if;
  end Is_Valid;

  -- Can source and children be moved on target
  function Can_Move (Source, Target : in Cards.Card_Access) return Boolean is
    Nb_To_Move, Nb_Movable : Positive;
    Nb_Available : Integer;
    Target_Empty_Free, Source_Top_Free : Boolean;
    use type Cards.Card_Access, Cards.Deck.Full_Suit_List;
  begin
    if Source.Stack.Name = Cards.Deck.Symbol_Name
    and then not Allow_From_Done then
      -- Cannot move from Done
      Logger.Log_Debug ("Can_Move -> FALSE cause from Done stack");
      return False;
    end if;
    -- Must be valid,
    if not Is_Valid (Source, Target) then
      Logger.Log_Debug ("Can_Move -> FALSE cause not valid");
      return False;
    end if;

    if Target.Stack.Name = Cards.Deck.Symbol_Name
    or else not Cards.Is_Play_Stack (Target.Stack) then
      -- Cards go to Done or Tmp one by one
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
    -- Nb children compatible with number of empty stacks
    Nb_To_Move := Source.Nb_Children + 1;
    Logger.Log_Debug ("  Can_Move.To_Move = " & Nb_To_Move'Img);
    Nb_Available := Nb_Free_Play_Stacks;
    Logger.Log_Debug ("  Can_Move.Nb_Available = " & Nb_Available'Img);
    Target_Empty_Free := Target.Stack.Suit = Cards.Deck.Empty
                and then Target.Stack.Nb_Children = 0;
    Source_Top_Free := Source.Stack.Suit = Cards.Deck.Empty
              and then Source.Stack.Next = Source;
    if Target_Empty_Free then
      -- Target is an empty free stack, which cannot be used
      Logger.Log_Debug ("  Can_Move.Nb_Available decreased");
      Nb_Available := Nb_Available - 1;
    end if;
    Logger.Log_Debug ("  Can_Move.Nb_Free_Tmp = " & Nb_Free_Tmp_Stacks'Img);
    -- (T + A) + (T + A-1) + ... + (T + 1) + T + 1
    Nb_Movable := Nb_Available * (Nb_Available + 1) / 2
                + Nb_Free_Tmp_Stacks * Nb_Available + Nb_Free_Tmp_Stacks + 1;
    if Target_Empty_Free and then Source_Top_Free
    and then Nb_Movable /= 1
    and then Nb_Available /= 0 then
      -- Source is top of a free stack and target is an empty free stack
      Nb_Movable := Nb_Movable + 1;
      Logger.Log_Debug ("  Can_Move.Nb_Movable increased");
    end if;
    Logger.Log_Debug ("  Can_Move.Nb_Movable = " & Nb_Movable'Img);
    return Nb_To_Move <= Nb_Movable;
  end Can_Move;

  --------------
  -- Movement --
  --------------
  -- Internal: Move one card
  procedure Move_One (Mov : Movement; Add : in Boolean; Wait : in Boolean) is
    Curr, Prev, Stack, Parent : Cards.Card_Access;
    Valid, Movable : Boolean;
    Nb_Children : Natural;
    use type Cards.Card_Access, Cards.Deck.Full_Suit_List;
  begin
    -- Adjust the stack and top card of From
    Curr := Mov.Card;
    Prev := Mov.Card.Prev;
    Parent := Mov.Card.Prev;
    Stack := Mov.From;
    if Stack.Nb_Children = 1 then
      -- The stack becomes empty
      Stack.Next := null;
      Stack.Prev := null;
      if Stack.Suit = Cards.Deck.Empty then
        if Cards.Is_Play_Stack (Stack) then
          Nb_Free_Play_Stacks := Nb_Free_Play_Stacks + 1;
        else
          Nb_Free_Tmp_Stacks := Nb_Free_Tmp_Stacks + 1;
        end if;
      end if;
    else
      -- Prev becomes top of stack
      Prev.Next := null;
      Prev.Nb_Children := 0;
      Prev.Movable := True;
      Stack.Prev := Prev;
      -- Update parents in source stack
      if Stack.Suit = Cards.Deck.Empty and then Cards.Is_Play_Stack (Stack) then
        Curr := Curr.Prev;
        Prev := null;
        while Curr /= Stack loop
          if Prev = null then
            -- Top of stack is movable
            Valid := True;
            Movable := True;
            Nb_Children := 0;
          else
            Valid := Is_Valid (Prev, Curr);
            Movable := Movable and then Valid;
          end if;
          Curr.Nb_Children := Nb_Children;
          Curr.Movable := Movable;
          if Movable then
            Nb_Children := Nb_Children + 1;
          else
            Nb_Children := 0;
          end if;
          Prev := Curr;
          Curr := Curr.Prev;
        end loop;
      end if;
    end if;
    Stack.Nb_Children := Stack.Nb_Children - 1;

    -- Link to top of stack of To and to new parent
    Curr := Mov.Card;
    Prev := Curr.Prev;
    Stack := Mov.To;
    Curr.Stack := Stack;
    if Stack.Nb_Children = 0 then
      -- The stack is empty
      Stack.Next := Curr;
      Curr.Prev := Stack;
      if Stack.Suit = Cards.Deck.Empty then
        if Cards.Is_Play_Stack (Stack) then
          Nb_Free_Play_Stacks := Nb_Free_Play_Stacks - 1;
        else
          Nb_Free_Tmp_Stacks := Nb_Free_Tmp_Stacks - 1;
        end if;
      end if;
    else
      Stack.Prev.Next := Curr;
      Curr.Prev := Stack.Prev;
    end if;
    Stack.Prev := Curr;
    Stack.Nb_Children := Stack.Nb_Children + 1;

    -- Last adjustments
    if Stack.Suit = Cards.Deck.Empty then
      -- Move to play or tmp stack, including undo from Done stack
      Curr.Movable := True;
      -- Increment the number of children of each ancestor of a play stack
      if Cards.Is_Play_Stack (Stack) then
        loop
          Curr := Curr.Prev;
          exit when not Curr.Movable;
          Curr.Nb_Children := Curr.Nb_Children + 1;
        end loop;
      end if;
      if Parent.Stack.Suit /= Cards.Deck.Empty then
        -- If undo a move to Done stack, overwrite Movable of parent
        Parent.Movable := Allow_From_Done;
      end if;
    elsif not Allow_From_Done then
      -- Move to Done stack (direct or undo) => Irreversible
      Curr.Movable := False;
    end if;

    -- Move the X card
    Logger.Log_Debug ("Moving " & Mov.Card.Image & " to " & Stack.Image);
    if Stack.Suit = Cards.Deck.Empty then
      if Cards.Is_Play_Stack (Stack) then
        Mov.Card.Xcard.Move (Table.Play_Of (
            Play  => Stack.Name,
            Depth => Stack.Nb_Children) );
      else
        Mov.Card.Xcard.Move (Table.Tmp_Of (Tmp  => Stack.Name));
      end if;
    else
      Mov.Card.Xcard.Move (Table.Done_Of (Suit => Stack.Suit) );
    end if;
    if Add then
      Memory.Add (Mov);
    end if;
    Prev.Xcard.Show (True);
    Prev.Xcard.Do_Raise;
    Mov.Card.Xcard.Do_Raise;
    if Wait then
      Table.Console.Flush;
      Table.Wait (0.1);
    end if;
  end Move_One;

  -- Internal data for multiple move
  Available_Stacks : array (Cards.Play_Stack_Range) of Boolean;
  subtype Available_Range is Natural range 0 .. Cards.Play_Stack_Range'Last;
  Nb_Available : Available_Range;
  -- History of stacked cards
  package Cards_Lifo is new Queues.Lifo (Cards.Card_Access);
  Play_Lifo : Cards_Lifo.Lifo_Type (Queues.Size_Range
      (Cards.Play_Stack_Range'Last));
  Tmp_Lifo : Cards_Lifo.Lifo_Type (Queues.Size_Range
       (Cards.Tmp_Stack_Range'Last));
  -- The card that is ancestor of Tmp cards
  Tmp_Root : Cards.Card_Access;

  -- Internal: Find next available stack
  function Next_Available return Cards.Play_Stack_Range is
  begin
    for Stack in Cards.Play_Stack_Range loop
      if Available_Stacks(Stack) then
        Logger.Log_Debug ("    Next available => "
                        & Cards.The_Play(Stack).Image);
        return Stack;
      end if;
    end loop;
    raise Program_Error;
  end Next_Available;

  -- Internal: Adjust Stack_Free after a move
  procedure Adjust_Available (Source, Target : Cards.Card_Access) is
    use type Cards.Card_Access;
  begin
    if Source /= null and then Source.Nb_Children = 0 then
      -- Maybe opening a free stack
      Logger.Log_Debug ("    Adjust source => " & Source.Image);
      Available_Stacks(Source.Name) := True;
      Nb_Available := Nb_Available + 1;
    end if;
    if Target /= null and then Target.Nb_Children = 1 then
      Logger.Log_Debug ("    Adjust target => " & Target.Image);
      Available_Stacks(Target.Name) := False;
      Nb_Available := Nb_Available - 1;
    end if;
  end Adjust_Available;

  -- Internal: Move cards of Source stack into the free tmp stacks,
  -- Stop before card=Term or when Source stack has one element
  procedure Move_To_Tmp (Source, Term : in Cards.Card_Access;
                        Add : in Boolean) is
    Target, Child :  Cards.Card_Access;
    use type Cards.Card_Access;
  begin
    Logger.Log_Debug (" Moving to Tmp stacks");
    for I in Cards.Tmp_Stack_Range loop
      Target := Cards.The_Tmp(I)'Access;
      if Target.Nb_Children = 0 then
        Child := Source.Prev;
        exit when Child = Term;
        exit when Child = Source.Next;
        Tmp_Root := Child.Prev;
        Move_One ( (Card => Child, From => Source, To => Target), Add, True);
        Tmp_Lifo.Push (Child);
      end if;
    end loop;
    Logger.Log_Debug (" End move to Tmp stacks, root="
       & (if Tmp_Root = null then "null" else Tmp_Root.Image));
  end Move_To_Tmp;

  -- Internal: Move from Tmp stacks to Target stack
  procedure Move_From_Tmp (Target : in Cards.Card_Access; Add : in Boolean) is
    Source, Child :  Cards.Card_Access;
  begin
    Logger.Log_Debug (" Moving from Tmp stacks");
    for S in 1 .. Tmp_Lifo.Length loop
      Child := Tmp_Lifo.Pop;
      Source := Child.Stack;
      Move_One ( (Card => Child, From => Source, To => Target), Add, True);
    end loop;
    Tmp_Root := null;
    Logger.Log_Debug (" End move from Tmp stacks");
  end Move_From_Tmp;

  -- Internal: Move a stack of several cards
  procedure Move_Multiple (Mov : Movement; Add : in Boolean) is
    Available : Cards.Play_Stack_Range;
    Source, Target, Child :  Cards.Card_Access;
    Nb_Spread : Available_Range;
    Stack_Str : As.U.Asu_Us;
    use type Cards.Card_Access, Cards.Deck.Full_Suit_List;
  begin
    Logger.Log_Debug ("Start Multi Move of " & Mov.Card.Image
                      & " to " & Mov.To.Image);

    -- Identify the available play stacks (excluding dest)
    Nb_Available := 0;
    for Stack in Cards.Play_Stack_Range loop
      Available_Stacks(Stack) := Cards.The_Play(Stack).Nb_Children = 0
          and then Mov.To /= Cards.The_Play(Stack)'Access;
      if Available_Stacks(Stack) then
        Nb_Available := Nb_Available + 1;
      end if;
    end loop;
    Logger.Log_Debug ("Nb_Available:" & Nb_Available'Img);
    if Logger.Debug_On then
      Stack_Str.Set_Null;
      for Stack in Cards.Play_Stack_Range loop
        if Available_Stacks(Stack) then
          Stack_Str.Append (Stack'Img);
        end if;
      end loop;
      Logger.Log_Debug ("Available stacks:" & Stack_Str.Image);
    end if;

    -- Specific handling of a full "Empty" stack to an empty "Empty" stack
    -- We move N-1 cards, then the root, then we restack
    if Mov.To.Suit = Cards.Deck.Empty
    and then Mov.To.Nb_Children = 0
    and then Mov.From.Suit = Cards.Deck.Empty
    and then Mov.From.Next = Mov.Card
    and then Mov.Card.Next /= null
    and then Nb_Available /= 0 then
      Logger.Log_Debug ("Full stack to free empty => 3 moves");
      -- Find a tempo available stack for N-1 (not To)
      Available := Next_Available;
      -- Move N-1 to tempo stack
      Child := Mov.Card.Next;
      Target := Cards.The_Play(Available)'Access;
      Move_Multiple (Mov => (Card => Child,
                             From => Mov.From,
                             To   => Target),
                     Add => Add);
      -- Move root to target
      Move_One (Mov, Add, True);
      -- Move N-1 to top of root
      Move_Multiple (Mov => (Card => Child,
                             From => Target,
                             To   => Mov.To),
                     Add => Add);
      return;
    end if;

    -- Debug summary of multi move
    if Logger.Debug_On then
      Child := Mov.Card;
      Stack_Str.Set_Null;
      loop
        Stack_Str.Append (" " & Child.Image);
        exit when Child.Next = null;
        Child := Child.Next;
      end loop;
      Logger.Log_Debug (Images.Integer_Image (Mov.Card.Nb_Children + 1)
                      & " cards:" & Stack_Str.Image
                      & ", with " & Images.Integer_Image (Nb_Free_Tmp_Stacks)
                      & " free tmp stacks, and "
                      & Images.Integer_Image (Nb_Available)
                      & " available play stacks");
    end if;

    -- Move children into free play stacks
    Play_Lifo.Clear;
    Tmp_Lifo.Clear;
    Tmp_Root := null;
    Logger.Log_Debug (" Starting spreading");
    Spread :
    loop
      Source := Mov.Card.Stack;
      -- Move children from top to Tmp
      Move_To_Tmp (Source, Mov.Card, Add);
      -- Move children from top, one per free stack
      Nb_Spread := 0;
      loop
        -- Move top if not the card to move and while some Available
        Child := Source.Prev;
        if Child = Mov.Card then
          Logger.Log_Debug (" Reached the card to move");
          exit Spread;
        end if;
        if Nb_Available = 0 then
          Logger.Log_Debug ("  No more available stack");
          exit;
        end if;
        -- Move top card to available play stack
        Available := Next_Available;
        Target := Cards.The_Play(Available)'Access;
        Logger.Log_Debug ("  Descending " & Child.Image
                        & " in stack " & Target.Image);
        Move_One ( (Card => Child, From => Source, To => Target), Add, True);
        Adjust_Available (Source, Target);
        Play_Lifo.Push (Child);
        Nb_Spread := Nb_Spread + 1;
      end loop;
      Logger.Log_Debug (" Spread " &  Images.Integer_Image (Nb_Spread)
                      & " cards");
      -- Stack all these children on the first one
      Target := Play_Lifo.Pop.Stack;
      for I in 1 .. Nb_Spread - 1 loop
        Child := Play_Lifo.Pop;
        Source := Child.Stack;
        Logger.Log_Debug ("  Restacking " & Child.Image
                        & " into stack " &  Target.Image);
        Move_One ( (Card => Child, From => Source, To => Target), Add, True);
        Adjust_Available (Source, Target);
      end loop;

      -- Unstack from Tmp to this substack
      Move_From_Tmp (Target, Add);
      -- Push this completed sub-stack
      Play_Lifo.Push (Target);
    end loop Spread;
    Logger.Log_Debug (" Finished spreading into "
                    & Long_Longs.Image (Play_Lifo.Length) & " stacks");

    -- Move the card to target
    Logger.Log_Debug ("  Moving " & Mov.Card.Image & "  to target");
    Move_One (Mov, Add, True);
    -- Don't decrement target if it is a free stack
    Adjust_Available (Source => Mov.From,
      Target => (if Mov.To.Next = Mov.Card then null else Mov.To) );

    -- Pop the stacks to target
    Logger.Log_Debug (" Starting unspreading");
    Target := Mov.To;
    -- Unstack from Tmp to target if they are children of it
    if Tmp_Root = Mov.Card then
      Move_From_Tmp (Target, Add);
    end if;
    for S in 1 .. Play_Lifo.Length loop
      -- Move all but last children, from top, one per free stack
      Source := Play_Lifo.Pop.Stack;
      Logger.Log_Debug (" Unstacking stack " & Source.Image);
      Move_To_Tmp (Source, null, Add);
      Child := Source.Prev;
      Nb_Spread := 0;
      while Child /= Source.Next loop
        Available := Next_Available;
        Target := Cards.The_Play(Available)'Access;
        Logger.Log_Debug ("  Descending " & Child.Image
                        & " in stack " & Target.Image);
        Move_One ( (Card => Child, From => Source, To => Target), Add, True);
        Adjust_Available (Source, Target);
        Play_Lifo.Push (Child);
        Nb_Spread := Nb_Spread + 1;
        Child := Source.Prev;
      end loop;
      Play_Lifo.Push (Child);
      Nb_Spread := Nb_Spread + 1;
      -- Move these children to target
      Target := Mov.To;
      for I in 1 .. Nb_Spread loop
        Child := Play_Lifo.Pop;
        Source := Child.Stack;
        Logger.Log_Debug ("  Moving " & Child.Image & "  to target");
        Move_One ( (Card => Child, From => Source, To => Target), Add, True);
        Adjust_Available (Source, Target);
      end loop;
      if Tmp_Root = Child then
        Move_From_Tmp (Target, Add);
      end if;
    end loop;

    Logger.Log_Debug ("End Multi Move");
  end Move_Multiple;

  -- Move some cards
  procedure Move (Mov : Movement; Add : in Boolean) is
    use type Cards.Card_Access;
  begin
    if Mov.Card.Next = null then
      -- One card to move
      Move_One (Mov, Add, False);
    else
      -- Several cards
      Move_Multiple (Mov, Add);
    end if;
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
            Mov := (Card => Acc, From => Acc.Stack, To => Target.Stack);
            Move_One (Mov, True, True);
            One_Moved := True;
          else
            exit Depth;
          end if;
          Acc := Acc.Prev;
        end loop Depth;
      end loop Play_Stacks;
      Tmp_Stacks:
      for I in Cards.Tmp_Stack_Range loop
        Acc := Cards.The_Tmp(I).Next;
        if Acc /= null and then Can_Be_Purged (Acc) then
          Target := Cards.The_Done(Acc.Suit)'Access;
          if Target.Nb_Children /= 0 then
            Target := Target.Prev;
          end if;
          Mov := (Card => Acc, From => Acc.Stack, To => Target.Stack);
          Move_One (Mov, True, True);
          One_Moved := True;
        end if;
      end loop Tmp_Stacks;
      exit Iter when not One_Moved;
    end loop Iter;
  end Purge;

end Movements;

