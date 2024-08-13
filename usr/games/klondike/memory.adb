with Dynamic_List, Rnd, Limited_Pool, Trace.Loggers, My_Math;
with Cards, Table;
package body Memory is

  Logger : Trace.Loggers.Logger;

  -- Done and undone
  procedure Set (To : out Movements.Movement; Val : in Movements.Movement) is
  begin
    To := Val;
  end Set;
  package Movement_Pool is new Limited_Pool (Movements.Movement,
                                             Lifo => True, Set => Set);
  Dones, Undones : Movement_Pool.Pool_Type;

  -- Clear the list of undos and redos
  procedure Clear is
  begin
    Dones.Clear;
    Undones.Clear;
  end Clear;

  ----------
  -- Game --
  ----------
  -- Initial setup
  Nb_Suites : constant := Cards.Deck.Suit_List'Pos(Cards.Deck.Suit_List'Last)
                       -  Cards.Deck.Suit_List'Pos(Cards.Deck.Suit_List'First)
                       + 1;
  subtype Card_Index is Positive
                        range 1 .. Nb_Suites * Cards.Deck.Name_Range'Last;
  type Setup is array (Card_Index) of Cards.Card_Access;
  Current_Game : Setup;
  Max_Depth : constant := 7;
  subtype Depth_Range is Positive range 1 .. Max_Depth;
  Depth_Of : constant array (Cards.Play_Stack_Range) of Depth_Range
           :=  (1, 2, 3, 4, 5, 6, 7);

  -- Update the links and counters, after start or restore
  procedure Update is
    Acc, Child, Top : Cards.Card_Access;
    Index, Tmp_Index : Card_Index;
  begin
    Index := Card_Index'First;
    for Stack in Cards.Play_Stack_Range loop
      Logger.Log_Debug ("Stack:" & Stack'Img);
      Cards.The_Play(Stack).Stack := Cards.The_Play(Stack)'Access;
      Cards.The_Play(Stack).Nb_Children := Depth_Of (Stack);
      -- Move and show X card
      for Depth in Depth_Range range 1 .. Depth_Of (Stack) loop
        Acc := Current_Game (Index);
        -- Move X card
        Acc.Xcard.Move (Table.Play_Of (Stack, Depth));
        Acc.Xcard.Show (True);
        Acc.Xcard.Do_Raise;
        Index := Index + 1;
      end loop;
      -- Last card of stack
      Tmp_Index := Index;
      for Depth in reverse Depth_Range range 1 .. Depth_Of (Stack) loop
        Tmp_Index := Tmp_Index - 1;
        Acc := Current_Game (Tmp_Index);
        Acc.Stack := Cards.The_Play(Stack)'Access;
        -- Link card with child
        if Depth = Depth_Of (Stack) then
          -- Top
          Acc.Xcard.Turn_Over (Face_Up => True);
          Acc.Movable := True;
          Child := null;
          Top := Acc;
        else
          Acc.Xcard.Turn_Over (Face_Up => False);
          Acc.Movable := False;
          Child.Prev := Acc;
        end if;
        -- Update tags
        Acc.Next := Child;
        Acc.Nb_Children := 0;
        -- Update for next
        Logger.Log_Debug ("  " & Acc.Image & ": "
            & Acc.Movable'Img & Acc.Nb_Children'Img);
        Child := Acc;
      end loop;
      -- Link first card with stack
      Acc.Prev := Cards.The_Play(Stack)'Access;
      Cards.The_Play(Stack).Prev := Top;
      Cards.The_Play(Stack).Next := Acc;
    end loop;

    -- Reset Done stacks
    for Suit in Cards.Deck.Suit_List loop
      Cards.The_Done(Suit).Prev := null;
      Cards.The_Done(Suit).Next := null;
      Cards.The_Done(Suit).Stack := Cards.The_Done(Suit)'Access;
      Cards.The_Done(Suit).Nb_Children := 0;
    end loop;

    -- Init dock stacks, show top
    for I in Cards.Dock_Range loop
      Cards.The_Dock(I).Stack := Cards.The_Dock(I)'Access;
    end loop;
    Child := null;
    Cards.The_Dock(Cards.Dock_Reserve).Nb_Children :=
        Card_Index'Last - Index + 1;
    for I in Index .. Card_Index'Last loop
      Acc := Current_Game (I);
      -- Move X card to dock Reserve, face down
      Acc.Xcard.Move (Table.Dock_Of (Cards.Dock_Reserve));
      Acc.Xcard.Turn_Over (Face_Up => False);
      -- Link cards with stack and cards together
      Acc.Stack := Cards.The_Dock(Cards.Dock_Reserve)'Access;
      if I = Index then
        Cards.The_Dock(Cards.Dock_Reserve).Next := Acc;
      elsif I = Card_Index'Last then
        Cards.The_Dock(Cards.Dock_Reserve).Prev := Acc;
        Child.Next := Acc;
      else
        Child.Next := Acc;
      end if;
      Acc.Prev := Child;
      -- Card will move alone
      Acc.Nb_Children := 0;
      Child := Acc;
      Logger.Log_Debug ("  Reserve " & Acc.Image);
    end loop;
    -- Show (back of) top of dock reserve
    Acc.Movable := True;
    Acc.Xcard.Show (True);
    Acc.Xcard.Do_Raise;
    -- Stack Reserve is clickable (but will not move)
    Cards.The_Dock(Cards.Dock_Reserve).Movable := True;

    -- Init Dock Pull stack, empty
    Cards.The_Dock(Cards.Dock_Pull).Prev := null;
    Cards.The_Dock(Cards.Dock_Pull).Next := null;
    Cards.The_Dock(Cards.Dock_Pull).Stack :=
        Cards.The_Dock(Cards.Dock_Pull)'Access;
    Cards.The_Dock(Cards.Dock_Pull).Nb_Children := 0;

  end Update;

  -- Dynamic list of cards for random init
  package Cards_Dyn_List_Mng is new Dynamic_List (Cards.Card_Access);
  package Cards_List_Mng renames Cards_Dyn_List_Mng.Dyn_List;
  Cards_List : Cards_List_Mng.List_Type;

 -- Game_num management
  Max_Real : constant My_Math.Real :=  My_Math.Real (Req_Game_Range'Last + 1);

  -- Initialise and save a new game
  procedure Start_Game (Num : in Req_Game_Range) is
    R : Positive;
    Acc : Cards.Card_Access;
    Moved : Boolean;
    Game_Num : Game_Range;
    use type My_Math.Real;
  begin
     Logger.Init ("Memory");
     -- Build a list of cards
    for Suit in Cards.Deck.Suit_List loop
      for Name in Cards.Deck.Name_Range loop
        Cards_List.Insert (Cards.The_Cards(Suit, Name)'Access);
      end loop;
    end loop;

    -- Set Game num
    if Num = Random_Num then
      -- Random game num
      Rnd.Gen.Randomize;
      Game_Num := Game_Range (My_Math.Trunc (
         My_Math.Real (Rnd.Gen.Float_Random) * Max_Real));
    else
      Game_Num := Num;
    end if;
    Rnd.Gen.Randomize (Float (Game_Num) / Float (Max_Real));
    Table.Set_Game_Num (Game_Num);

    -- Put and link the cards
    for I in Card_Index loop
      R := Rnd.Gen.Int_Random (1, Cards_List.List_Length);
      Cards_List.Move_At (R);
      Cards_List.Get (Acc, Moved => Moved);
      -- Store in setup
      Current_Game(I) := Acc;
    end loop;
    -- Remaining cards in the pool
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
  -- Add a movement to list of undoes
  -- Clears the list of redoes
  procedure Add (Mov : in Movements.Movement) is
  begin
    Dones.Push (Mov);
    Undones.Clear;
  end Add;

  -- Pop a movement and add it to the list of redoes
  function Can_Undo return Boolean is (not Dones.Is_Empty);

  function Undo return Movements.Movement is
  begin
    return Mov : Movements.Movement do
      Dones.Pop (Mov);
      Undones.Push (Mov);
    end return;
  end Undo;

  -- Pop a undone movment and add it to the list of undoes
  function Can_Redo return Boolean is (not Undones.Is_Empty);
  function Redo return Movements.Movement is
  begin
    return Mov : Movements.Movement do
      Undones.Pop (Mov);
      Dones.Push (Mov);
    end return;
  end Redo;

  -- Clear the redoes
  procedure Clear_Redoes is
  begin
    Undones.Clear;
  end Clear_Redoes;

end Memory;

