with Con_Io, Argument, Basic_Proc, Environ;
with Cards, Table, Memory, Movements;
procedure Freecell is
  Event : Table.Event_Rec;
  Mov : Movements.Movement;
  Stack, Card : Cards.Card_Access;
  Game_Num : Memory.Req_Game_Range := Memory.Random_Num;
  Invalid_Argument : exception;

  type Status_List is (None, Selectable, Selected, Targetable, Targeted);
  Status : Status_List := None;
  Selected_Source, Selected_Target, Tmp_Card : Cards.Card_Access := null;

  use type Cards.Card_Access;
  -- Un-select the selected source card and reset status to None
  --  (before scrambling/Undo/Redo)
  procedure Reset is
  begin
    Status := None;
    if Selected_Source /= null then
      Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
      Selected_Source.Xcard.Un_Select;
      Selected_Source := null;
    end if;
  end Reset;

  use type Cards.Deck.Full_Suit_List;
begin
  -- Optional game num
  begin
    if Argument.Get_Nbre_Arg = 0 then
      null;
    elsif Argument.Get_Nbre_Arg = 1 then
      Game_Num := Memory.Game_Range'Value (Argument.Get_Parameter);
    else
      raise Invalid_Argument;
    end if;
  exception
    when others =>
      raise Invalid_Argument;
  end;
  -- Global init
  Table.Init;

  -- Init game
  Movements.Reset;
  Memory.Start_Game (Game_Num);

  -- Play game
  loop
    Table.Next_Event (Event);
    case Event.Kind is
      when Table.Quit =>
        -- End of game
        exit;
      when Table.New_Game =>
        Reset;
        Movements.Reset;
        Memory.Start_Game (Memory.Random_Num);
      when Table.Start =>
        Game_Num := Table.Get_Num;
        Reset;
        Movements.Reset;
        if Game_Num = Memory.Random_Num then
          -- Invalid_Num
          Memory.Restore_Game;
        else
          Memory.Start_Game (Game_Num);
        end if;
        Table.Reset_Num;
      when Table.Purge =>
        -- Save selection
        Tmp_Card := Selected_Source;
        Reset;
        Movements.Purge;
        -- Restore selection if it has not been purged in Done stack
        if Tmp_Card /= null
        and then Tmp_Card.Stack.Suit = Cards.Deck.Empty then
          Selected_Source := Tmp_Card;
          Tmp_Card.Xcard.Do_Select;
          Status := Selected;
        end if;
      when Table.Undo =>
        if Memory.Can_Undo then
          Reset;
          Mov := Memory.Undo;
          Movements.Move ( Mov => (Card => Mov.Card,
                                   From => Mov.To,
                                   To   => Mov.From),
                           Add => False );
        end if;
      when Table.Redo =>
        Reset;
        if Memory.Can_Redo then
          Reset;
          Movements.Move (Memory.Redo, Add => False);
        end if;
      when Table.Enter =>
        case Status is
          when None =>
            if Event.Card.Movable then
              -- Entering a selectable source
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
              Status := Selectable;
            end if;
          when Selectable =>
            -- Impossible, we must leave it first, and become None
            null;
          when Selected =>
            if Movements.Can_Move (Selected_Source, Event.Card) then
              -- Entering a eligible target
              Table.Console.Set_Pointer_Shape (Con_Io.Target);
              Event.Card.Xcard.Do_Select;
              Status := Targetable;
            end if;
          when Targetable | Targeted =>
            -- Impossible, we must leave it first, and become Selected
            null;
        end case;
      when Table.Leave =>
        case Status is
          when None =>
            -- Leaving a non movable card
            null;
          when Selectable =>
            -- Leaving a selectable source
            Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            Status := None;
          when Selected =>
            -- Leaving a selected source
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
          when Targetable =>
            -- Leaving an eligible target
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            Event.Card.Xcard.Un_Select;
            Status := Selected;
          when Targeted =>
            -- Leaving a selected target
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            Event.Card.Xcard.Un_Select;
            Selected_Target := null;
            Status := Selected;
        end case;
      when Table.Left_Pressed =>
        case Status is
          when None =>
            -- Pressing in a non movable card
            null;
          when Selectable =>
            -- Left pressing a selectable source => toggle select
            if Event.Card /= Selected_Source then
              Event.Card.Xcard.Do_Select;
              Selected_Source := Event.Card;
              Status := Selected;
            end if;
          when Selected =>
            -- Pressing again the selected card => unselect
            if Event.Card = Selected_Source then
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
              Event.Card.Xcard.Un_Select;
              Selected_Source := null;
              Status := Selectable;
            end if;
          when Targetable =>
            -- Pressing in an eligible target
            Selected_Target := Event.Card;
            Status := Targeted;
          when Targeted =>
            -- Impossible, we must leave or release first
            null;
        end case;
      when Table.Left_Released =>
        case Status is
          when None =>
            -- Releasing in a non movable card
            null;
          when Selectable =>
            -- Releasing in a selectable card
            null;
          when Selected =>
            -- Releasing in the selected source
            null;
          when Targetable =>
            -- Impossible
            null;
          when Targeted =>
            -- Releasing in Selected target
            -- Set movement
            Mov := (Card => Selected_Source,
                    From => Selected_Source.Stack,
                    To   => Selected_Target.Stack);
            -- Unselect
            Reset;
            Status := Selectable;
            Selected_Target.Xcard.Un_Select;
            Selected_Target := null;
            -- Move
            Movements.Move (Mov, True);
        end case;
      when Table.Right_Pressed =>
        -- Right click-release
        if Status = Selectable
        or else (Status = Selected
                 and then Selected_Source = Event.Card) then
          -- Try to move to Done
          Stack := Cards.The_Done(Event.Card.Suit)'Access;
          if Stack.Prev /= null then
            Card := Stack.Prev;
          else
            Card := Stack;
          end if;
          if not Movements.Can_Move (Event.Card, Card) then
            -- Try to find a free Tmp stack
            Stack := null;
            for I in Cards.Tmp_Stack_Range loop
              if Cards.The_Tmp(I).Nb_Children = 0
              and then  Movements.Can_Move (Event.Card,
                                            Cards.The_Tmp(I)'Access) then
                Stack := Cards.The_Tmp(I)'Access;
                exit;
              end if;
            end loop;
          end if;
          if Stack /= null then
            Mov := (Card => Event.Card,
                    From => Event.Card.Stack,
                    To   => Stack);
            -- Save Prev
            Reset;
            Card := Event.Card.Prev;
            Movements.Move (Mov, True);
            -- Is cursor now on a card
            if Card = null
            or else Card.Suit = Cards.Deck.Empty
            or else not Table.Is_Pointer_Above (Card) then
              Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            else
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            end if;
          end if;
        end if;
      when Table.Right_Released =>
        null;
    end case;
  end loop;
exception
  when Invalid_Argument =>
    Basic_Proc.Put_Line_Error ("ERROR: Invalid argument.");
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ <game_number> ]     // 0 .. 999999");
  when others =>
    declare
      Val : constant String := Environ.Getenv ("FREECELL_WAIT_EXCEPTION");
    begin
      if Val /= "" then
        delay Duration'Value (Val);
      end if;
    end;
    raise;
end Freecell;

