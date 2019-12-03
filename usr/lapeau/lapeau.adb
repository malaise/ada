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
      Basic_Proc.Put_Line_Output (Event.Kind'Img & " " & Event.Card.Image
          & (if Event.Card.Movable then " MOVABLE" else ""));
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
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
              Status := Selectable;
            end if;
          when Selectable =>
            -- Impossible
            null;
          when Selected =>
            if Movements.Can_Move (Selected_Card, Event.Card, False) then
              -- Entering a eligible target
              Event.Card.Xcard.Do_Select;
              Status := Targetable;
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
            Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            if Event.Card.Xcard.Is_Selected  then
              -- It was pressed
              Event.Card.Xcard.Un_Select;
            end if;
            Status := None;
          when Selected =>
            -- Leaving a non eligible target
            null;
          when Targetable =>
            -- Leaving an eligible target
            Event.Card.Xcard.Un_Select;
            Status := Selected;
        end case;
      when Table.Pressed =>
        case Status is
          when None =>
            -- Pressing in a non movable card
            null;
          when Selectable =>
            -- Pressing a movable card => toggle select
            if Event.Card /= Selected_Card then
              Event.Card.Xcard.Do_Select;
              Selected_Card := Event.Card;
              Status := Selected;
            end if;
          when Selected =>
            -- Pressing again the selected card
            if Event.Card = Selected_Card then
              Event.Card.Xcard.Un_Select;
              Selected_Card := null;
              Status := Selectable;
            end if;
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
            null;
          when Selected =>
            -- Releasing in the selected source
            null;
          when Targetable =>
            -- Releasing in eligible target
            Status := None;
            Selected_Card.Xcard.Un_Select;
            Event.Card.Xcard.Un_Select;
            -- Move
            Basic_Proc.Put_Line_Output ("Move...");
            Movements.Move ( (
                Card => Selected_Card,
                From => Selected_Card.Stack,
                To   => Event.Card.Stack) );
            Selected_Card := null;
        end case;
    end case;
   Basic_Proc.Put_Line_Output (Status'Img);
  end loop;
end Lapeau;

