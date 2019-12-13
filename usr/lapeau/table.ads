with Con_Io;
with Cards;
package Table is

  package Deck renames Cards.Deck;

  -- The main window
  Console : aliased Con_Io.Console;

  -- Needs to be called only once, create the table, move the stacks, cards, menu...
  procedure Init;

  -- Stacks --
  subtype Stack_Range is Deck.Name_Range;
  -- 4 Cards, the last one being the Ace of a complete color
  subtype Depth_Range is Natural range 1 .. 3 + Deck.Name_Range'Last;

  -- Number of empty stacks, from 0 to 13-9
  subtype Empty_Stacks_Range is Natural range 0 .. 9;
  Empty_Stacks : Empty_Stacks_Range;

  -- Position (X, Y) of card within a stack
  function Stack_Of (Stack : Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec;
  -- Position (X, Y) of card within a done
  function Done_Of (Suit : Deck.Suit_List) return Deck.Position_Rec;

  -- Is the pointer currently above a card
  --  Based only on positions (the card might be covered or hidden)
  function Is_Pointer_Above (Acard : Cards.Card_Access) return Boolean;

  -- Events --
  -- Wait infinitely for next event, on card, on menu or Break
  type Event_List is (Left_Pressed, Left_Released,
                      Right_Pressed, Right_Released,
                      Enter, Leave,
                      Quit, New_Game, Restart, Purge, Undo, Redo);
  subtype Card_Event_List is Event_List range Left_Pressed .. Leave;
  subtype Menu_Event_List is Event_List range Quit .. Redo;
  type Event_Rec (Kind : Event_List:= Quit) is record
    case Kind is
      when Card_Event_List =>
        Card : Cards.Card_Access;
      when Menu_Event_List =>
        null;
    end case;
  end record;
  procedure Next_Event (Event : out Event_Rec);

  -- Wait some milliseconds
  procedure Wait (Dur : in Duration);

end Table;

