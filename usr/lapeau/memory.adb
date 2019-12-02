with Dynamic_List, Rnd;
with Cards, Table;
package body Memory is

  -- Clear the list of undos and redos
  procedure Clear is
  begin
    -- @@@
    null;
  end Clear;

  ----------
  -- Game --
  ----------
  -- Initial setup
  Max_Depth : constant := 4;
  subtype Depth_Range is Positive range 1 .. Max_Depth;
  type Setup is array (Table.Stack_Range, Depth_Range) of Cards.Card_Access;
  Current_Game : Setup;

  -- Upadte the links and counters, after start or restore
  procedure Update is
    Acc, Child, Top : Cards.Card_Access;
    Movable, Valid : Boolean;
    Nb_Children : Natural;
    use type Cards.Card_Access;
  begin
    for Stack in Table.Stack_Range loop
      -- Move card
      for Depth in Depth_Range loop
        Acc := Current_Game (Stack, Depth);
        -- Move X card
        Acc.Xcard.Move (Table.Stack_Of (Stack, Depth));
        Acc.Xcard.Do_Raise;
      end loop;
      -- Last child of stack
      Child := null;
      Nb_Children := 0;
      for Depth in reverse Depth_Range loop
        Acc := Current_Game (Stack, Depth);
        Acc.Stack := Cards.The_Stacks(Stack)'Access;
        -- Link card with child
        if Depth = Depth_Range'Last then
          -- Top
          Acc.Next := null;
          Top := Acc;
          Valid := False;
          Movable := True;
        else
          Acc.Next := Child;
          Child.Prev := Acc;
          Valid := Movements.Is_Valid (Child, Acc);
          Movable := Movable and then Valid;
        end if;
        -- Update tags
        Acc.Nb_Children := Nb_Children;
        Acc.Movable := Movable;
        if Valid then
          Nb_Children := Nb_Children + 1;
        else
          Nb_Children := 0;
        end if;
        -- Update for next
        Child := Acc;
      end loop;
      -- Link first card with stack
      Acc.Prev := Cards.The_Stacks(Stack)'Access;
      Cards.The_Stacks(Stack).Prev := Top;
      Cards.The_Stacks(Stack).Next := Acc;
      Cards.The_Stacks(Stack).Stack := Cards.The_Stacks(Stack)'Access;
      Cards.The_Stacks(Stack).Nb_Children := 4;
    end loop;
    -- Reset Done stacks
    for Suit in Cards.Deck.Suit_List loop
      Cards.The_Dones(Suit).Prev := null;
      Cards.The_Dones(Suit).Next := null;
      Cards.The_Dones(Suit).Stack := Cards.The_Dones(Suit)'Access;
      Cards.The_Dones(Suit).Nb_Children := 0;
    end loop;
  end Update;

  -- Dynamic list of cards for random init
  package Cards_Dyn_List_Mng is new Dynamic_List (Cards.Card_Access);
  package Cards_List_Mng renames Cards_Dyn_List_Mng.Dyn_List;
  Cards_List : Cards_List_Mng.List_Type;

  -- Initialise and save a new game
  procedure Start_Game is
    R : Positive;
    Acc : Cards.Card_Access;
    Moved : Boolean;
  begin
     -- Build a list of cards
    for Suit in Cards.Deck.Suit_List loop
      for Name in Cards.Deck.Name_Range loop
        Cards_List.Insert (Cards.The_Cards(Suit, Name)'Access);
      end loop;
    end loop;

    -- Put and link the cards
    Rnd.Gen.Randomize;
    for Stack in Table.Stack_Range loop
      for Depth in Depth_Range loop
        R := Rnd.Gen.Int_Random (1, Cards_List.List_Length);
        Cards_List.Move_At (R);
        Cards_List.Get (Acc, Moved => Moved);
        -- Store in setup
        Current_Game(Stack, Depth) := Acc;
      end loop;
    end loop;
    -- Move cards and update counters and links
    Update;
    -- Clear history
    Clear;
  end Start_Game;

  -- Restore current game
  procedure Restore_Game is
  begin
    -- Move cards and update counters and links
    Update;
    -- Clear history
    Clear;
  end Restore_Game;

  -------------------------
  -- Movements undo/redo --
  -------------------------

  -- The lists are clear when a game starts
  -- Add a movement to list of undoes
  -- Clears the list of redoes
  procedure Add (Mov : in Movements.Movement) is
  begin
    -- @@@
    null;
  end Add;

  -- Pop a movement and add it to the list of redoes
  function Undo return Movements.Movement is
  begin
    -- @@@
    return (null, null, null);
  end Undo;

  -- Pop a undone movment and add it to the list of undoes
  function Redo return Movements.Movement is
  begin
    -- @@@
    return (null, null, null);
  end Redo;

end Memory;

