with Dynamic_List, Rnd;
with Movements;
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
    Acc, Child : Cards.Card_Access;
    Movable : Boolean;
    Nb_Children : Natural;
    use type Cards.Card_Access;
  begin
    for Stack in Table.Stack_Range loop
      -- Last child of stack
      Child := null;
      Movable := True;
      Nb_Children := 0;
      for Depth in reverse Depth_Range loop
        Acc := Current_Game (Stack, Depth);
        -- Link card with child
        Acc.Next := Child;
        if Depth = Depth_Range'Last then
          Acc.Next := null;
        else
          Child.Prev := Acc;
          Movable := Movable and then Movements.Is_Valid (Acc, Child);
        end if;
        -- Update tags
        Acc.Movable := Movable;
        Acc.Nb_Children := Nb_Children;
        if Movable then
          Nb_Children := Nb_Children + 1;
        end if;
        -- Update for next
        Child := Acc;
      end loop;
      -- Link first card with stack
      Cards.The_Stacks (Stack).Next := Acc;
      Acc.Prev := Cards.The_Stacks (Stack)'Access;
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
    for Stack in Table.Stack_Range loop
      for Depth in Depth_Range loop
        R := Rnd.Gen.Int_Random (1, Cards_List.List_Length);
        Cards_List.Move_At (R);
        Cards_List.Get (Acc, Moved => Moved);
        -- Move X card
        Acc.Xcard.Move (Table.Pos_Of (Stack, Depth));
        Acc.Xcard.Show (True);
        -- Store in setup
        Current_Game(Stack, Depth) := Acc;
      end loop;
    end loop;
    -- Update counters and links
    Update;
    -- Clear history
    Clear;
  end Start_Game;



  -- Restore current game
  procedure Restore_Game is
    Acc : Cards.Card_Access;
  begin
    -- Restore current game
    for Stack in Table.Stack_Range loop
      for Depth in Depth_Range loop
        Acc := Current_Game(Stack, Depth);
        -- Move X card
        Acc.Xcard.Move (Table.Pos_Of (Stack, Depth));
        Acc.Xcard.Show (True);
      end loop;
    end loop;
    -- Update counters and links
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
  procedure Add (Mov : in Movement) is
  begin
    -- @@@
    null;
  end Add;

  -- Pop a movement and add it to the list of redoes
  function Undo return Movement is
  begin
    -- @@@
    return (null, 1, 1);
  end Undo;

  -- Pop a undone movment and add it to the list of undoes
  function Redo return Movement is
  begin
    -- @@@
    return (null, 1, 1);
  end Redo;

end Memory;

