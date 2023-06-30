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
    -- For a stack (play or done), Stack is itself,
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
  subtype Play_Stack_Range is Positive range 1 .. 8;
  The_Xplay : array (Play_Stack_Range) of aliased Deck.Card;
  The_Play  : array (Play_Stack_Range) of aliased Card;

  -- X dones and our corresponding cards
  The_Xdone : array (Deck.Suit_List) of aliased Deck.Card;
  The_Done  : array (Deck.Suit_List) of aliased Card;

  -- X tmp and our corresponding cards
  subtype Tmp_Stack_Range is Positive range 1 .. 4;
  The_Xtmp : array (Tmp_Stack_Range) of aliased Deck.Card;
  The_Tmp  : array (Tmp_Stack_Range) of aliased Card;

  -- Init the deck and both lists of cards (needs to be called once)
  procedure Init (Line_Id : in X_Mng.Line);

  -- Get the card corresponding to a Xcard (by using its Suit and Name
  --  null if not found in cards or stacks
  function X_To_Card (Ref : X_Mng.External_Reference) return Card_Access;

  -- Is a stack a Play or a Tmp stack
  function Is_Play_Stack (Card : Card_Access) return Boolean;

  -- Is a stack a Done stack
  function Is_Done_Stack (Card : Card_Access) return Boolean;
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

