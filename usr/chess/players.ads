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

  -- Returns a not Valid Action when end of list
  procedure Rewind_Actions (Color : in Space.Color_List);
  function  Next_Action    (Color : Space.Color_List) return Action_Rec;

  -- Find an action (Move, Castle, Take, Take_En_Passant) matching
  --  from and to. Not valid if not found.
  -- No more than one should match
  More_Than_One : exception;
  function Find_Action (Color : in Space.Color_List;
                        From, To : Space.Square_Coordinate) return Action_Rec;
  
end Players;

