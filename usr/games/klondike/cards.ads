with X_Mng.Cards;
package Cards is

  ------------------ -
  -- Deck of cards --
  ------------------ -
  package Deck is new X_Mng.Cards;

  -- Local definition of card
  type Card;
  type Card_Access is access all Card;
  type Card is tagged limited record
    -- These fields are set once for all at init
    Xcard : Deck.Card_Access;
    Suit : Deck.Full_Suit_List := Deck.Empty;
    Name : Deck.Full_Name_Range := 1;
    Image : String (1 .. 2) := "  ";
    -- Only these fields are modified
    -- For a stack (play, dock or done), Stack is itself,
    --  Next is the first, Prev is the top
    --  and Nb_Children is the Nb of cards
    -- For a Done stack,
    Prev, Next, Stack : Card_Access := null;
    Nb_Children : Natural := 0;
    Movable : Boolean := False;
  end record;

  -- X cards and our corresponding cards
  The_Xcards : array (Deck.Suit_List, Deck.Name_Range) of aliased Deck.Card;
  The_Cards  : array (Deck.Suit_List, Deck.Name_Range) of aliased Card;

  -- X stacks and our corresponding cards
  subtype Play_Stack_Range is Positive range 1 .. 7;
  The_Xplay : array (Play_Stack_Range) of aliased Deck.Card;
  The_Play  : array (Play_Stack_Range) of aliased Card;

  -- X dones and our corresponding cards
  The_Xdone : array (Deck.Suit_List) of aliased Deck.Card;
  The_Done  : array (Deck.Suit_List) of aliased Card;

  -- X dock (Reserve and Pull) and our corresponding cards
  subtype Dock_Range is Positive range 1 .. 2;
  Dock_Reserve : constant Dock_Range := 1;
  Dock_Pull : constant Dock_Range := 2;
  The_Xdock : array (Dock_Range) of aliased Deck.Card;
  The_Dock  : array (Dock_Range) of aliased Card;

  -- Init the deck and both lists of cards (needs to be called once)
  procedure Init (Line_Id : in X_Mng.Line);

  -- Get the card corresponding to a Xcard (by using its Suit and Name
  --  null if not found in cards or stacks
  function X_To_Card (Ref : X_Mng.External_Reference) return Card_Access;

  -- Is a stack a Done stack
  function Is_Done_Stack (Stack : Card_Access) return Boolean;
  -- Is a stack a Play stack
  function Is_Play_Stack (Stack : Card_Access) return Boolean;
  -- Is a stack the Reserve stack
  function Is_Reserve_Stack (Stack : Card_Access) return Boolean;

  -- Image of a card access
  function Image (Card : Card_Access) return String;

  ----------------------
  -- Suits and Colors --
  ----------------------
  -- Color of a suit
  type Colors is (Red, Black);
  Color_Of : constant array (Deck.Suit_List) of Colors
           := (Deck.Heart | Deck.Diamond => Red,
               Deck.Club  | Deck.Spade   => Black);

  -- Suits of a color
  type Suits_Pair is array (1 .. 2) of Deck.Suit_List;
  Suits_Of : constant array (Colors) of Suits_Pair
           := (Red => (Deck.Heart,  Deck.Diamond),
               Black => (Deck.Club, Deck.Spade) );

  -- Alternate color of a color
  Alternate_Color : array (Colors) of Colors
                  := (Red   => Black,
                      Black => Red);

end Cards;

