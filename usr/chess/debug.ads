with Space, Pieces, Players;
package Debug is

  type Debug_List is (Moves, Think, No_Check);

  procedure Set (Kind : in Debug_List; On : in Boolean);

  function Get (Kind : in Debug_List) return Boolean;

  -- True if at least one debug is on
  function Some return Boolean;

  procedure Put (Square : in Space.Square_Coordinate);
  procedure Put (Piece : in Pieces.Basic_Piece'Class);
  procedure Put (Action : in Pieces.Action_Rec);
  procedure Put (Action : in Players.Action_Rec);

end Debug;

