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
      The_Xdones(Suit).Create_Symbol (Suit);
      The_Dones(Suit).Xcard := The_Xdones(Suit)'Access;
      The_Dones(Suit).Suit := Suit;
      The_Dones(Suit).Name := Deck.Symbol_Name;
      The_Dones(Suit).Image := "d" & Image (Suit);
    end loop;

    -- Create the stacks
    for Name in Deck.Name_Range loop
      The_Xstacks(Name).Create_Empty (Name, Squared => False);
      The_Stacks(Name).Xcard := The_Xstacks(Name)'Access;
      The_Stacks(Name).Suit := Deck.Empty;
      The_Stacks(Name).Name := Name;
      The_Stacks(Name).Image := Normal (Name, 2, Gap => '0');
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
      return The_Cards(Acc.Get_Suit, Acc.Get_Name)'Access;
    elsif Acc.Is_Symbol then
      return The_Dones(Acc.Get_Suit)'Access;
    else
      return The_Stacks(Acc.Get_Name)'Access;
    end if;
  end X_To_Card;

end Cards;

