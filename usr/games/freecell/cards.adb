with Int_Img, Normal;
package body Cards is

  function Image (Name : Deck.Name_Range) return Character is
  begin
    return (case Name is
      -- Returns a string (2 .. Last)
      when 1 .. 9 => Int_Img (Name)(2),
      when 10     => 'a',
      when 11     => 'J',
      when 12     => 'Q',
      when 13     => 'K');
  end Image;

  function Image (Suit : Deck.Suit_List) return Character is
  begin
    return (case Suit is
      when Deck.Club    => 'C',
      when Deck.Spade   => 'S',
      when Deck.Heart   => 'H',
      when Deck.Diamond => 'D');
  end Image;

  Initialized : Boolean := False;
  procedure Init (Line_Id : in X_Mng.Line) is
  begin
    if Initialized then
      return;
    end if;
    Deck.Set_Line (Line_Id, Enable_Motion => False);
    -- Create the cards and the done stacks
    for Suit in Deck.Suit_List loop
      for Name in Deck.Name_Range loop
        The_Xcards(Suit, Name).Create_Card (Suit, Name);
        The_Cards(Suit, Name).Xcard := The_Xcards(Suit, Name)'Access;
        The_Cards(Suit, Name).Suit := Suit;
        The_Cards(Suit, Name).Name := Name;
        The_Cards(Suit, Name).Image := Image (Name) & Image (Suit);
      end loop;
      The_Xdone(Suit).Create_Symbol (Suit);
      The_Done(Suit).Xcard := The_Xdone(Suit)'Access;
      The_Done(Suit).Suit := Suit;
      The_Done(Suit).Name := Deck.Symbol_Name;
      The_Done(Suit).Image := "d" & Image (Suit);
    end loop;

    -- Create the playing and tmp stacks
    for I in Play_Stack_Range loop
      The_Xplay(I).Create_Empty (I, Squared => False);
      The_Play(I).Xcard := The_Xplay(I)'Access;
      The_Play(I).Suit := Deck.Empty;
      The_Play(I).Name := I;
      The_Play(I).Image := "p" & Normal (I, 1);
    end loop;
    for I in  Tmp_Stack_Range loop
      The_Xtmp(I).Create_Empty (Play_Stack_Range'Last + I, Squared => True);
      The_Tmp(I).Xcard := The_Xtmp(I)'Access;
      The_Tmp(I).Suit := Deck.Empty;
      The_Tmp(I).Name := I;
      The_Tmp(I).Image := "t" & Normal (I, 1);
    end loop;

    Initialized := True;
  end Init;

  -- Get the card corresponding to a Xcard (by using its Suit and Name
  function X_To_Card (Ref : X_Mng.External_Reference) return Card_Access is
    Acc : Deck.Card_Access;
    use type X_Mng.External_Reference;
  begin
    if Ref = X_Mng.Null_Reference then
      return null;
    end if;
    Acc := Deck.Ref_To_Access (Ref);
    if Acc.Is_Card then
      -- Real card
      return The_Cards(Acc.Get_Suit, Acc.Get_Name)'Access;
    elsif Acc.Is_Symbol then
      -- Done stack
      return The_Done(Acc.Get_Suit)'Access;
    elsif Acc.Get_Name in Play_Stack_Range then
      -- Play stack
      return The_Play(Acc.Get_Name)'Access;
    else
      -- Tmp stack
      return The_Tmp(Acc.Get_Name - Play_Stack_Range'Last)'Access;
    end if;
  end X_To_Card;

  -- Is a stack a Done stack
  function Is_Done_Stack (Card : Card_Access) return Boolean is
  begin
    return Card.Xcard.Is_Symbol;
  end Is_Done_Stack;

  -- Otherwise, is it a Play stack (or a Tmp stack)
  function Is_Play_Stack (Card : Card_Access) return Boolean is
  begin
    return Card.Xcard.Get_Name in Play_Stack_Range;
  end Is_Play_Stack;
  -- Image of a card access
  function Image (Card : Card_Access) return String is
  (if Card  /= null then Card.Image else "");

end Cards;

