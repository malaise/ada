with X_Mng.Cards;
package Cards is

  -- Deck of cards
  package Deck is new X_Mng.Cards;

  -- Local definition of card
  type Card;
  type Card_Access is access all Card;
  type Card is tagged limited record
    -- These fields are set once for all at init
    Xcard : Deck.Card_Access;
    Suit : Deck.Full_Suit_List := Deck.Empty;
    Name : Deck.Name_Range := 1;
    Image : String (1 .. 2) := "  ";
    -- Only these fields are modified
    Prev, Next, Top, Bottom : Card_Access := null;
    Nb_Children : Natural := 0;
    Movable : Boolean := False;
  end record;

  -- X cards and our corresponding cards
  The_Xcards : array (Deck.Suit_List, Deck.Name_Range) of aliased Deck.Card;
  The_Cards  : array (Deck.Suit_List, Deck.Name_Range) of aliased Card;

  -- X stacks and our corresponding cards
  subtype Stack_Range is Deck.Name_Range;
  The_Xstacks : array (Stack_Range) of aliased Deck.Card;
  The_Stacks  : array (Stack_Range) of aliased Card;

  -- Init the deck and both lists of cards (needs to be called once)
  procedure Init (Line_Id : in X_Mng.Line);

  -- Get the card corresponding to a Xcard (by using its Suit and Name
  --  null if not found in cards or stacks
  function X_To_Card (Ref : X_Mng.External_Reference) return Card_Access;

end Cards;

