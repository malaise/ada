with Con_Io;
with Cards, Memory;
package Table is

  package Deck renames Cards.Deck;

  -- The main window
  Console : aliased Con_Io.Console;

  -- Needs to be called only once, create the table, move the stacks, cards,
  --  menu...
  procedure Init;

  -- Set game num
  procedure Set_Game_Num (Num : in Memory.Game_Range);

  -- Stacks --
  -- 7 Cards, the last one being the Ace of a complete color
  subtype Depth_Range is Natural range 1 .. 7 + Deck.Name_Range'Last;

  -- Position (X, Y) of card within a play stack
  function Play_Of (Play : Cards.Play_Stack_Range; Depth : Depth_Range)
           return Deck.Position_Rec;
  -- Position (X, Y) of card within a done stack
  function Done_Of (Suit : Deck.Suit_List) return Deck.Position_Rec;
  -- Position (X, Y) of card within a tmp stack
  function Tmp_Of (Tmp : Cards.Tmp_Stack_Range) return Deck.Position_Rec;

  -- Is the pointer currently above a card
  --  Based only on positions (the card might be covered or hidden)
  function Is_Pointer_Above (Acard : Cards.Card_Access) return Boolean;

  -- Events --
  -- Wait infinitely for next event, on card, on menu or Break
  type Event_List is (Left_Pressed, Left_Released,
                      Right_Pressed, Right_Released,
                      Enter, Leave,
                      Quit, New_Game, Start, Purge, Undo, Redo,
                      Num);
  subtype Card_Event_List is Event_List range Left_Pressed .. Leave;
  subtype Menu_Event_List is Event_List range Quit .. Redo;
  type Event_Rec (Kind : Event_List:= Quit) is record
    case Kind is
      when Card_Event_List =>
        Card : Cards.Card_Access;
      when Menu_Event_List =>
        null;
      when Num =>
        Col : Con_Io.Col_Range;
    end case;
  end record;
  procedure Next_Event (Event : out Event_Rec);

  -- Wait some milliseconds
  procedure Wait (Dur : in Duration);

  -- Get / reset game num field
  function Get_Num return Memory.Req_Game_Range;
  procedure Reset_Num;

end Table;

