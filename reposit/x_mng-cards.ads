generic
package X_Mng.Cards is

  ----------------------
  -- Line association --
  ----------------------
  -- Link this card deck to a X_Mng Line, which is often open after the
  --  instanciation of the gneeric (this is why it is not a formal generic
  --  parameter)
  -- This association is not reversible
  -- Enable pointer motion reporting for the cards, or not
  -- Raises X_Failure if Line_Id is not open, or if an association has already
  --  been set
  procedure Set_Line (Line_Id : in Line; Enable_Motion : in Boolean);

  -----------------------
  -- Cards definition --
  -----------------------
  Width : constant := 79;
  Height : constant := 123;
  -- Empty slot or symbol (Suit slot) or real card
  type Full_Suit_List is (Heart, Diamond, Club, Spade, Empty);
  for Full_Suit_List use (Heart => 0, Diamond => 1, Club => 2, Spade => 3,
                           Empty => 4);
  subtype Suit_List is Full_Suit_List range Heart .. Spade;

  -- Symbol (0) for empty or symbol target stack
  -- then from 1 to 10, then Jack, Queen and Kind
  subtype Full_Name_Range is Natural range 0 .. 13;
  subtype Name_Range is Full_Name_Range range 1 .. Full_Name_Range'Last;
  Symbol_Name : constant Full_Name_Range := 0;

  -- Any card, slot or symbol
  type Card is tagged limited private;
  type Card_Access is access all Card;

  ------------------------------
  -- Creation, Is_A, Deletion --
  ------------------------------
  -- Create an empty untyped slot, squared or not
  -- The Name is only for easier retrieval of the card when event
  procedure Create_Empty (Acard : in out Card; Name : Name_Range;
                          Squared : in Boolean);
  -- True for Empty slot, Squared or not
  function Is_Empty (Acard : Card) return Boolean;
  -- True for Empty Quared slot
  function Is_Squared (Acard : Card) return Boolean;

  -- Create a Symbol
  procedure Create_Symbol (Acard : in out Card; Suit : Suit_List);
  function Is_Symbol (Acard : Card) return Boolean;

  -- Create a card
  procedure Create_Card (Acard : in out Card; Suit : Suit_List;
                         Name : Name_Range);
  function Is_Card (Acard : Card) return Boolean;

  -- Get the Suit of a card
  function Get_Suit (Acard : Card) return Full_Suit_List;

  -- Get the name of a card or empty
  -- Raises Symbol_Error if the Card Suit is a Symbol
  function Get_Name (Acard : Card) return Full_Name_Range;

  -- Delete any Empty, Symbol or Card
  procedure Delete (Acard : in out Card);

  ------------
  -- Status --
  ------------
  -- Show or hide a card (show if not fully covered, not removed...)
  procedure Show (Acard : in out Card; Do_Show : in Boolean);
  function Is_Shown (Acard : Card) return Boolean;

  -- Turn over the card face up or down (if down we see its back)
  -- Raises Empty_Error if the Card Suit is Empty
  -- Raises Symbol_Error if the Card Suit is a Symbol
  procedure Turn_Over (Acard : in out Card; Face_Up : in Boolean);
  -- Is the card face up
  function Is_Face_Up (Acard : Card) return Boolean;

  -- (Un) select a card
  -- Raises Empty_Error if the Card Suit is Empty
  -- Raises Symbol_Error if the Card Suit is a Symbol
  procedure Do_Select (Acard : in out Card);
  procedure Un_Select (Acard : in out Card);
  -- Is the card selected
  function Is_Selected (Acard : Card) return Boolean;

  -- Position of top left corner from top left of window
  type Position_Rec is record
    X, Y : Natural;
  end record;
  procedure Move (Acard : in out Card; Position : in Position_Rec);
  function Get_Position (Acard : Card) return Position_Rec;

  -- Redispay or update the display of the card
  procedure Redisplay (Acard : in Card);

  -- From external reference to Card access
  function Ref_To_Access (Ref : External_Reference) return Card_Access;

  ----------------
  -- Exceptions --
  ----------------
  -- Internal errot
  Internal_Error : exception;
  -- When invalid operation on an empty slot
  Empty_Error : exception;
  -- When invalid operation on an symbol slot
  Symbol_Error : exception;

private

  type Card is tagged limited record
    Suit : Full_Suit_List := Empty;
    Squared : Boolean := False;
    Name : Full_Name_Range := Symbol_Name;
    Shown    : Boolean := False;  -- We don't see the card if it is not Shown
    Face_Up  : Boolean := True;   -- We see the back if the card is not Up
    Selected : Boolean := False;  -- Show special color (for a real card)
    Position : Position_Rec := (0, 0);
    Ccard : System.Address := System.Null_Address;
  end record;

end X_Mng.Cards;

