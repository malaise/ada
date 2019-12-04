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
  subtype Stack_Range is Deck.Name_Range;
  The_Xstacks : array (Stack_Range) of aliased Deck.Card;
  The_Stacks  : array (Stack_Range) of aliased Card;

  -- X dones and our corresponding cards
  The_Xdones : array (Deck.Suit_List) of aliased Deck.Card;
  The_Dones  : array (Deck.Suit_List) of aliased Card;

  -- Init the deck and both lists of cards (needs to be called once)
  procedure Init (Line_Id : in X_Mng.Line);

  -- Get the card corresponding to a Xcard (by using its Suit and Name
  --  null if not found in cards or stacks
  function X_To_Card (Ref : X_Mng.External_Reference) return Card_Access;

  -- Color of a suit
  type Colors is (Red, Black);
  Color_Of : constant array (Deck.Suit_List) of Colors
           := (Deck.Heart | Deck.Diamond => Red,
               Deck.Club  | Deck.Spade   => Black);

end Cards;

