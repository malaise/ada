with Ada.Unchecked_Conversion;
package body X_Mng.Cards is
  use type System.Address;
  Null_Address : constant System.Address := System.Null_Address;

  -----------------
  -- C functions --
  -----------------
  function Init_Desk (Line_Id : Line_For_C; Enable_Motion : in C_Types.Bool)
                     return C_Types.Bool
    with Import => True, Convention => C, External_Name => "initDesk";
  function Create_Empty (Squared : C_Types.Bool; Ref : System.Address)
           return System.Address
    with Import => True, Convention => C, External_Name => "createEmpty";
  function Create_Symbol (Suit : C_Types.Int; Ref : System.Address)
           return System.Address
    with Import => True, Convention => C, External_Name => "createSymbol";
  function Create_Card (Suit : C_Types.Int; Value : Natural;
           Ref : System.Address) return System.Address
    with Import => True, Convention => C, External_Name => "createCard";
  procedure Delete_Card (Acard : in System.Address)
    with Import => True, Convention => C, External_Name => "deleteCard";
  procedure Map (Acard : in System.Address)
    with Import => True, Convention => C, External_Name => "map";
  procedure Unmap (Acard : in System.Address)
    with Import => True, Convention => C, External_Name => "unmap";
  procedure Move (Acard : in System.Address; X, Y : in Integer)
    with Import => True, Convention => C, External_Name => "move";
  procedure Do_Select (Acard : in System.Address)
    with Import => True, Convention => C, External_Name => "doSelect";
  procedure Un_Select (Acard : in System.Address)
    with Import => True, Convention => C, External_Name => "unSelect";
  procedure Turn_Over (Acard : in System.Address; Face_Up : in C_Types.Bool)
    with Import => True, Convention => C, External_Name => "turnOver";

  ----------------------
  -- Line association --
  ----------------------
  Line_Id : Line := No_Client;
  procedure Set_Line (Line_Id : in Line; Enable_Motion : in Boolean) is
    Line_For_C_Id : Line_For_C;
    use type C_Types.Bool;
  begin
    if X_Mng.Cards.Line_Id /= No_Client or else Line_Id = No_Client then
      raise X_Failure;
    end if;
    X_Mng.Cards.Line_Id := Line_Id;
    Call_On (X_Mng.Cards.Line_Id, Line_For_C_Id);
    if not Init_Desk (Line_For_C_Id, C_Types.Bool (Enable_Motion)) then
      Call_Off (X_Mng.Cards.Line_Id, Line_For_C_Id);
      raise X_Failure;
    end if;
    Call_Off (X_Mng.Cards.Line_Id, Line_For_C_Id);
  end Set_Line;

  Line_For_C_Id : Line_For_C;
  procedure Call_On is
  begin
    X_Mng.Call_On (Line_Id, Line_For_C_Id);
  end Call_On;
  procedure Call_Off is
  begin
    X_Mng.Call_Off (Line_Id, Line_For_C_Id);
  end Call_Off;

  ------------------------------
  -- Creation, Is_A, Deletion --
  ------------------------------
  -- Create an empty untyped slot
  procedure Create_Empty (Acard : in out Card; Squared : in Boolean) is
  begin
    Acard.Suit := Empty;
    Acard.Name := Symbol_Name;
    Acard.Squared := Squared;
    Call_On;
    Acard.Ccard := Create_Empty (C_Types.Bool (Squared), Acard'Address);
    Call_Off;
    if Acard.Ccard = Null_Address then
      raise Internal_Error;
    end if;
  end Create_Empty;

  function Is_Empty (Acard : Card) return Boolean is
  begin
    return Acard.Suit = Empty;
  end Is_Empty;

  function Is_Squared (Acard : Card) return Boolean is
  begin
    return Acard.Suit = Empty and then Acard.Squared;
  end  Is_Squared;

  -- Create a Symbol
  procedure Create_Symbol (Acard : in out Card; Suit : Suit_List) is
  begin
    Acard.Suit := Suit;
    Acard.Name := Symbol_Name;
    Call_On;
    Acard.Ccard := Create_Symbol (Suit_List'Pos (Suit), Acard'Address);
    Call_Off;
    if Acard.Ccard = Null_Address then
      raise Internal_Error;
    end if;
  end Create_Symbol;

  function Is_Symbol (Acard : Card) return Boolean is
  begin
    return Acard.Suit /= Empty and then Acard.Name = Symbol_Name;
  end Is_Symbol;

  -- Create a card
  procedure Create_Card (Acard : in out Card; Suit : Suit_List;
                         Name : Name_Range) is
  begin
    Acard.Suit := Suit;
    Acard.Name := Name;
    Call_On;
    Acard.Ccard := Create_Card (Suit_List'Pos (Suit), Name - 1,
                                Acard'Address);
    Call_Off;
    if Acard.Ccard = Null_Address then
      raise Internal_Error;
    end if;
  end Create_Card;

  function Is_Card (Acard : Card) return Boolean is
  begin
    return Acard.Suit /= Empty and then Acard.Name /= Symbol_Name;
  end Is_Card;

  -- Get the Suit of a card
  function Get_Suit (Acard : Card) return Full_Suit_List is
  begin
    return Acard.Suit;
  end Get_Suit;

  -- Get the name of a card
  -- Raises Empty_Error if the Card Suit is Empty
  function Get_Name (Acard : Card) return Full_Name_Range is
  begin
    if Is_Empty (Acard) then
      raise Empty_Error;
    end if;
    return Acard.Name;
  end Get_Name;

  -- Delete any Empty, Symbol or Card
  procedure Delete (Acard : in out Card) is
  begin
    Delete_Card (Acard.Ccard);
    Acard.Suit     := Empty;
    Acard.Squared  := False;
    Acard.Name     := Symbol_Name;
    Acard.Shown    := False;
    Acard.Face_Up  := True;
    Acard.Selected := False;
    Acard.Position := (0, 0);
    Acard.Ccard    := System.Null_Address;
  end Delete;

  ------------
  -- Status --
  ------------
  -- Show or hide a card (show if not fully covered, not removed...)
  procedure Show (Acard : in out Card; Do_Show : in Boolean) is
  begin
    if Do_Show then
      Map (Acard.Ccard);
    else
      Unmap (Acard.Ccard);
    end if;
    Acard.Shown := Do_Show;
  end Show;

  function Is_Shown (Acard : Card) return Boolean is
  begin
    return Acard.Shown;
  end Is_Shown;

  -- Turn over the card Up (otherwise we see its back)
  procedure Turn_Over (Acard : in out Card; Face_Up : in Boolean) is
  begin
    if Is_Empty (Acard) then
      raise Empty_Error;
    end if;
    if Is_Symbol (Acard) then
      raise Symbol_Error;
    end if;
    Turn_Over (Acard.Ccard, C_Types.Bool (Face_Up));
    Acard.Face_Up := Face_Up;
  end Turn_Over;

  -- Is the card face up
  function Is_Face_Up (Acard : Card) return Boolean is
  begin
    return Acard.Face_Up;
  end Is_Face_Up;

  -- (Un) select a card
  -- Raises Empty_Error if the Card Suit is Empty
  -- Raises Symbol_Error if the Card Suit is a Symbol
  procedure Do_Select (Acard : in out Card) is
  begin
    Do_Select (Acard.Ccard);
    Acard.Selected := True;
  end Do_Select;
  procedure Un_Select (Acard : in out Card) is
  begin
    Un_Select (Acard.Ccard);
    Acard.Selected := True;
  end Un_Select;
  -- Is the card selected
  function Is_Selected (Acard : Card) return Boolean is (Acard.Selected);


  -- Position of top left corner from top left of window
  -- type Position_Rec is record
  --   X, Y : Integer;
  -- end record;
  procedure Move (Acard : in out Card; Position : in Position_Rec) is
  begin
    Move (Acard.Ccard, C_Types.Int (Position.X), C_Types.Int (Position.Y));
    Acard.Position := Position;
  end Move;

  function Get_Position (Acard : Card) return Position_Rec is
  begin
    return Acard.Position;
  end Get_Position;

  -- Redispay or update the display of the card
  procedure Redisplay (Acard : in Card) is
  begin
    Map (Acard.Ccard);
  end Redisplay;

  -- From external reference to Card access
  function Ref_2_Acc is new Ada.Unchecked_Conversion (
      Source => External_Reference, Target => Card_Access);

  function Ref_To_Access (Ref : External_Reference) return Card_Access is
      (Ref_2_Acc (Ref));

end X_Mng.Cards;

