with Con_Io;
with Cards, Table, Memory, Movements, Basic_Proc;
procedure Lapeau is
  Event : Table.Event_Rec;
  type Status_List is (None, Selectable, Selected, Targetable);
  Status : Status_List := None;
  Selected_Card : Cards.Card_Access := null;
  use type Table.Event_List, Cards.Card_Access;
begin
  -- Global init
  Table.Init;

  -- Init game
  Memory.Start_Game;

  -- Play game
  loop
    Table.Next_Event (Event);
    if Event.Kind in Table.Pressed .. Table.Leave then
      Basic_Proc.Put_Line_Output (Event.Kind'Img & " " & Event.Card.Image);
    end if;
    case Event.Kind is
      when Table.Quit =>
        -- End of game
        exit;
      when Table.New_Game =>
        Memory.Start_Game;
      when Table.Restart =>
        Memory.Restore_Game;
      when Table.Undo .. Table.Redo =>
         -- @@@ handle menu
         null;
      when Table.Enter =>
        case Status is
          when None =>
            if Event.Card.Movable then
              -- Entering a movable source
              Status := Selectable;
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            end if;
          when Selectable =>
            -- Impossible
            null;
          when Selected =>
            if Movements.Can_Move (Selected_Card, Event.Card) then
              -- Entering a eligible target
              Status := Targetable;
              Event.Card.Xcard.Do_Select;
            end if;
          when Targetable =>
            -- Impossible
            null;
        end case;
      when Table.Leave =>
        case Status is
          when None =>
            -- Leaving a non movable card
            null;
          when Selectable =>
            -- Leaving a movable source
            Status := None;
            Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            if Event.Card.Xcard.Is_Selected  then
              -- It was pressed
              Event.Card.Xcard.Un_Select;
            end if;
          when Selected =>
            -- Leaving a non eligible target
            null;
          when Targetable =>
            -- Leaving an eligible target
            Status := Selected;
            Event.Card.Xcard.Un_Select;
        end case;
      when Table.Pressed =>
        case Status is
          when None =>
            -- Pressing in a non movable card
            null;
          when Selectable =>
            -- Pressing a movable card
            -- Waiting for release
            Event.Card.Xcard.Do_Select;
          when Selected =>
            -- Pressing in a non eligible target
            null;
          when Targetable =>
            -- Pressing in an eligible target
            null;
        end case;
      when Table.Released =>
        case Status is
          when None =>
            -- Releasing in a non movable card
            null;
          when Selectable =>
            -- Releasing in a selectable card
            if Event.Card.Xcard.Is_Selected  then
              Status := Selected;
              Selected_Card := Event.Card;
            end if;
          when Selected =>
            -- Releasing in the selected source
            if Event.Card = Selected_Card then
              Status := Selectable;
              Event.Card.Xcard.Un_Select;
              Selected_Card := null;
            end if;
            -- Releasing in a non eligible target
          when Targetable =>
            -- Releasing in eligible target
            Status := None;
            Selected_Card.Xcard.Un_Select;
            Event.Card.Xcard.Un_Select;
            -- Move
            Basic_Proc.Put_Line_Output ("Move...");
            Movements.Move ( (
                Card => Selected_Card,
                From => Selected_Card.Stack.Name,
                To   => Event.Card.Stack.Name) );
            Selected_Card := null;
        end case;
    end case;
   Basic_Proc.Put_Line_Output (Status'Img);
  end loop;
end Lapeau;

