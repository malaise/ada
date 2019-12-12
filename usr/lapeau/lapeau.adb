with Con_Io, Argument;
with Cards, Table, Memory, Movements;
procedure Lapeau is
  Event : Table.Event_Rec;
  Mov : Movements.Movement;
  Stack, Card : Cards.Card_Access;

  type Status_List is (None, Selectable, Selected, Targetable, Targeted);
  Status : Status_List := None;
  Selected_Source, Selected_Target, Tmp_Card : Cards.Card_Access := null;

  use type Cards.Card_Access;
  -- Un-select the selected source card and reset status to None
  --  (before scrambling/UNdo/Redo)
  procedure Reset is
  begin
    Status := None;
    if Selected_Source /= null then
      Selected_Source.Xcard.Un_Select;
      Selected_Source := null;
    end if;
  end Reset;

  use type Cards.Deck.Full_Suit_List, Table.Event_List;
begin
  -- Adjust play stacking policy
  if Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter = "--alternate" then
    Movements.Stack_Policy := Movements.Alternate_Color;
  end if;

  -- Global init
  Table.Init;

  -- Init game
  Movements.Reset;
  Memory.Start_Game;

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
        Memory.Start_Game;
      when Table.Restart =>
        Reset;
        Movements.Reset;
        Memory.Restore_Game;
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
              -- Entering a movable source
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
              Status := Selectable;
            end if;
          when Selectable =>
            -- Impossible, we must leave it first, and become None
            null;
          when Selected =>
            if Movements.Can_Move (Selected_Source, Event.Card) then
              -- Entering a eligible target
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
            null;
          when Targetable =>
            -- Leaving an eligible target
            Event.Card.Xcard.Un_Select;
            Status := Selected;
          when Targeted =>
            -- Leaving a selected target
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
            -- Pressing a movable card => toggle select
            if Event.Card /= Selected_Source then
              Event.Card.Xcard.Do_Select;
              Selected_Source := Event.Card;
              Status := Selected;
            end if;
          when Selected =>
            -- Pressing again the selected card
            if Event.Card = Selected_Source then
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
            Selected_Source.Xcard.Un_Select;
            Selected_Target.Xcard.Un_Select;
            Status := None;
            -- Move
            Mov := (Card => Selected_Source,
                    From => Selected_Source.Stack,
                    To   => Selected_Target.Stack);
            Movements.Move (Mov, True);
            Selected_Source := null;
            Selected_Target := null;
        end case;
      when Table.Right_Pressed =>
        if Status = Selectable then
          Stack := Cards.The_Dones (Event.Card.Suit)'Access;
          if Stack.Prev /= null then
             Card := Stack.Prev;
          else
             Card := Stack;
          end if;
          if Movements.Can_Move (Event.Card, Card) then
            Status := None;
            Mov := (Card => Event.Card,
                    From => Event.Card.Stack,
                    To   => Stack);
            Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            Movements.Move (Mov, True);
          end if;
        end if;
      when Table.Right_Released =>
        null;
    end case;
  end loop;
end Lapeau;

