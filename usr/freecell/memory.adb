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
  Max_Depth : constant := 7;
  subtype Depth_Range is Positive range 1 .. Max_Depth;
  type Setup is array (Cards.Play_Stack_Range, Depth_Range)
             of Cards.Card_Access;
  Current_Game : Setup;
  Depth_Of : constant array (Cards.Play_Stack_Range) of Depth_Range
           :=  (1 .. 4 => 7, 5 .. 8 => 6);

  -- Upadte the links and counters, after start or restore
  procedure Update is
    Acc, Child, Top : Cards.Card_Access;
    Movable, Valid : Boolean;
    Nb_Children : Natural;
  begin
    for Stack in Cards.Play_Stack_Range loop
      Logger.Log_Debug ("Stack:" & Stack'Img);
      Cards.The_Play(Stack).Stack := Cards.The_Play(Stack)'Access;
      Cards.The_Play(Stack).Nb_Children := Depth_Of (Stack);
      -- Move and show X card
      for Depth in Depth_Range range 1 .. Depth_Of (Stack) loop
        Acc := Current_Game (Stack, Depth);
        -- Move X card
        Acc.Xcard.Move (Table.Play_Of (Stack, Depth));
        Acc.Xcard.Show (True);
        Acc.Xcard.Do_Raise;
      end loop;
      -- Last card of stack
      for Depth in reverse Depth_Range range 1 .. Depth_Of (Stack) loop
        Acc := Current_Game (Stack, Depth);
        Acc.Stack := Cards.The_Play(Stack)'Access;
        -- Link card with child
        if Depth = Depth_Of (Stack) then
          -- Top
          Child := null;
          Nb_Children := 0;
          Top := Acc;
          Valid := False;
          Movable := True;
        else
          Child.Prev := Acc;
          Valid := Movements.Is_Valid (Child, Acc);
          Movable := Movable and then Valid;
        end if;
        -- Update tags
        Acc.Next := Child;
        Acc.Movable := Movable;
        if Valid then
          Nb_Children := Nb_Children + 1;
        else
          Nb_Children := 0;
        end if;
        Acc.Nb_Children := Nb_Children;
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
    -- Reset Tmp stacks
    for I in Cards.Tmp_Stack_Range loop
      Cards.The_Tmp(I).Prev := null;
      Cards.The_Tmp(I).Next := null;
      Cards.The_Tmp(I).Stack := Cards.The_Tmp(I)'Access;
      Cards.The_Tmp(I).Nb_Children := 0;
    end loop;
    -- Reset Done stacks
    for Suit in Cards.Deck.Suit_List loop
      Cards.The_Done(Suit).Prev := null;
      Cards.The_Done(Suit).Next := null;
      Cards.The_Done(Suit).Stack := Cards.The_Done(Suit)'Access;
      Cards.The_Done(Suit).Nb_Children := 0;
    end loop;

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
    for Stack in Cards.Play_Stack_Range loop
      for Depth in Depth_Range range 1 .. Depth_Of (Stack) loop
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

