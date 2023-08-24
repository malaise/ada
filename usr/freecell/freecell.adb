with Con_Io, Argument, Basic_Proc, Environ, As.U, Trace.Loggers;
with Cards, Table, Memory, Movements;
procedure Freecell is
  Logger : Trace.Loggers.Logger;
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

  -- Save selection, purge and restore selection
  procedure Purge is
  begin
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
  end Purge;

  use type Table.Event_List;

begin
  -- Init traces to debug
  Trace.Init_Env (
    (As.U.Tus ("X_Mng"), As.U.Tus ("Event_Mng"),
     As.U.Tus ("Main"), As.U.Tus ("Memory"),
     As.U.Tus ("Movements"), As.U.Tus ("Table")),
    "Debug", "/tmp/Freecell.log");
  Logger.Init ("Main");

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
    Logger.Log_Debug ("Status: " & Status'Img & " "
        & "SelSrc: " & Cards.Image (Selected_Source) & " "
        & "SelTrg: " & Cards.Image (Selected_Target) & " "
        & "Event: " & Event.Kind'Img & " "
        & (case Event.Kind is
           when Table.Card_Event_List => Event.Card.Image,
           when Table.Menu_Event_List => "",
           when Table.Num => Event.Col'Img) );
    case Event.Kind is
      when Table.Quit =>
        -- End of game
        exit;
      when Table.New_Game =>
        Reset;
        Movements.Reset;
        Memory.Start_Game (Memory.Random_Num);
      when Table.Num =>
        -- Table adjusts cursor pos
        null;
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
        Purge;
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
              Logger.Log_Debug ("  movable card becomes selectable");
            else
              Logger.Log_Debug ("  drop cause unmovable card");
            end if;
          when Selectable =>
            -- Impossible, we must leave it first, and become None
            Logger.Log_Debug ("  drop cause status");
          when Selected =>
            if Movements.Can_Move (Selected_Source, Event.Card) then
              -- Entering a eligible target
              Table.Console.Set_Pointer_Shape (Con_Io.Target);
              Event.Card.Xcard.Do_Select;
              Status := Targetable;
              Logger.Log_Debug ("  card can move and becomes targetable");
            else
              Logger.Log_Debug ("  drop cause card cannot move");
            end if;
          when Targetable | Targeted =>
            -- Impossible, we must leave it first, and become Selected
            Logger.Log_Debug ("  drop cause status");
        end case;
      when Table.Leave =>
        case Status is
          when None =>
            -- Leaving a non movable card
            Logger.Log_Debug ("  drop cause status");
          when Selectable =>
            -- Leaving a selectable source
            Table.Console.Set_Pointer_Shape (Con_Io.Arrow);
            Status := None;
            Logger.Log_Debug ("  selectable card becomes normal");
          when Selected =>
            -- Leaving a selected source
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            Logger.Log_Debug ("  selected card remains selected");
          when Targetable =>
            -- Leaving an eligible target
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            Event.Card.Xcard.Un_Select;
            Status := Selected;
            Logger.Log_Debug ("  targetable card becomes normal");
          when Targeted =>
            -- Leaving a selected target
            Table.Console.Set_Pointer_Shape (Con_Io.Hand);
            Event.Card.Xcard.Un_Select;
            Selected_Target := null;
            Status := Selected;
            Logger.Log_Debug ("  targeted card becomes normal");
        end case;
      when Table.Left_Pressed =>
        case Status is
          when None =>
            -- Pressing in a non movable card
            Logger.Log_Debug ("  drop cause status");
          when Selectable =>
            -- Left pressing a selectable source => toggle select
            if Event.Card /= Selected_Source then
              Event.Card.Xcard.Do_Select;
              Selected_Source := Event.Card;
              Status := Selected;
              Logger.Log_Debug ("  selectable card becomes selected");
            else
              Logger.Log_Debug ("  drop cause card is the source");
            end if;
          when Selected =>
            -- Pressing again the selected card => unselect
            if Event.Card = Selected_Source then
              Table.Console.Set_Pointer_Shape (Con_Io.Hand);
              Event.Card.Xcard.Un_Select;
              Selected_Source := null;
              Status := Selectable;
              Logger.Log_Debug ("  selected card becomes selectable");
            else
              Logger.Log_Debug ("  drop cause card is the source");
            end if;
          when Targetable =>
            -- Pressing in an eligible target
            Selected_Target := Event.Card;
            Status := Targeted;
            Logger.Log_Debug ("  targetable card becomes targeted");
          when Targeted =>
            -- Impossible, we must leave or release first
           Logger.Log_Debug ("  drop cause status");
        end case;
      when Table.Left_Released =>
        case Status is
          when None =>
            -- Releasing in a non movable card
            Logger.Log_Debug ("  drop cause status");
          when Selectable =>
            -- Releasing in a selectable card
            Logger.Log_Debug ("  drop cause status");
          when Selected =>
            -- Releasing in the selected source
            Logger.Log_Debug ("  drop cause status");
          when Targetable =>
            -- Impossible
            Logger.Log_Debug ("  drop cause status");
          when Targeted =>
            -- Releasing in Selected target
            -- Set movement
            if Selected_Source /= null and then Selected_Target /= null then
              Mov := (Card => Selected_Source,
                      From => Selected_Source.Stack,
                      To   => Selected_Target.Stack);
              Tmp_Card := Selected_Target;
              -- Unselect
              Reset;
              Status := Selectable;
              Selected_Target.Xcard.Un_Select;
              Selected_Target := null;
              -- Move
              if Movements.Can_Move (Mov.Card, Tmp_Card) then
                Movements.Move (Mov, True);
              else
                Reset;
                Logger.Log_Debug ("  drop cause card cannot move");
              end if;
            else
              Reset;
              Logger.Log_Debug ("  drop cause no selected source or target");
            end if;

        end case;
      when Table.Right_Pressed | Table.Double_Click =>
        if Event.Card /= null
        and then Event.Card.Suit /= Cards.Deck.Empty
        and then (Status = Selectable
          or else (Status = Selected
                 and then Selected_Source = Event.Card)) then
          -- Try to move to Done
          Stack := Cards.The_Done(Event.Card.Suit)'Access;
          if Stack /= null and then Stack.Prev /= null then
            Card := Stack.Prev;
          else
            Card := Stack;
          end if;
          if Stack /= null
          and then not Movements.Can_Move (Event.Card, Card) then
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
          else
            Logger.Log_Debug ("  drop cause card cannot move");
          end if;
        elsif Event.Kind = Table.Double_Click
        and then Event.Card /= null
        and then Cards.Is_Done_Stack (Event.Card.Stack) then
          -- Double click on a Done stack
          Purge;
        else
          Logger.Log_Debug ("  drop cause cannot purge");
        end if;
      when Table.Right_Released =>
        Logger.Log_Debug ("  drop right release");
    end case;

    -- Unselect after double click
    if Event.Kind = Table.Double_Click
    and then Event.Card /= null
    and then Event.Card.Xcard.Is_Selected then
      Event.Card.Xcard.Un_Select;
      Logger.Log_Debug ("  Unselect after douvle click");
    end if;

    Logger.Log_Debug ("Status is now " & Status'Img);

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

