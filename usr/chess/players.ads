with Space, Pieces;
package Players is

  -- Build list of possible movements
  procedure Think (Color : in Space.Color_List);

  type Action_Rec (Valid : Boolean := False) is record
    case Valid is
      when True =>
        From : Space.Square_Coordinate := Space.Origin;
        To   : Pieces.Action_Rec;
      when False => null;
    end case;
  end record;

  -- Sequential access : Returns a not Valid Action when end of list
  procedure Rewind_Actions (Color : in Space.Color_List);
  function  Next_Action    (Color : Space.Color_List) return Action_Rec;

  -- Direct access :  Returns a not Valid Action when not in list
  function Get_Action (Color : Space.Color_List; Index : Positive) return Action_Rec;

  -- Find an action (Move, Castle, Take, Take_En_Passant, Promote, Take_And_Promote) matching
  --  from and to. Not valid if not found.
  -- Promotions are searched if (and only if) Promote is a Promotion_Piece_List
  -- No more than one should match
  More_Than_One : exception;
  function Find_Action (Color : Space.Color_List;
                        From, To : Space.Square_Coordinate;
                        Promote  : in Pieces.Piece_Kind_List) return Action_Rec;
  
end Players;

