with Space, Pieces;
package Team is

  -- Add a new piece to a team
  procedure Add (Piece : in Pieces.Piece_Access);

  -- Delete a piece from a team
  procedure Del (Piece : in Pieces.Piece_Access);

  -- Move to first piece of a team
  procedure Rewind (Color : in Space.Color_List);
  -- Get current piece of a team (or null) and to move to next
  function Get (Color : Space.Color_List) return Pieces.Piece_Access;

end Team;

