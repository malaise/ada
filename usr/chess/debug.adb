with Ada.Text_Io;
with Lower_Str, Normal;
with Space.Board;
package body Debug is

  Debug_Status : array (Debug_List) of Boolean := (others => False);

  procedure Set (Kind : in Debug_List; On : in Boolean) is
  begin
    Debug_Status(Kind) := On;
  end Set;

  function Get (Kind : in Debug_List) return Boolean is
  begin
    return Debug_Status(Kind);
  end Get;

  function Some return Boolean is
  begin
    for K in Debug_List loop
      if Get (K) then
        return True;
      end if;
    end loop;
    return False;
  end Some;

  procedure Put (Square : in Space.Square_Coordinate) is
  begin
    Ada.Text_Io.Put (Lower_Str(Space.Col_Range'Image(Square.Col))
                   & Normal(Integer(Square.Row), 1) );
  exception
    when others =>
      Ada.Text_Io.Put ("Exception when putting square");
  end Put;

  procedure Put (Piece : in Pieces.Basic_Piece'Class) is
    Id : constant Pieces.Piece_Id := Pieces.Id_Of (Piece);
  begin
    Ada.Text_Io.Put (Space.Color_List'Image(Id.Color)
         & " " & Pieces.Piece_Kind_List'Image(Id.Kind));
  exception
    when others =>
      Ada.Text_Io.Put ("Exception when putting piece");
  end Put;

  procedure Put (Action : in Pieces.Action_Rec) is
    use Ada.Text_Io;
    use type Space.Square_Coordinate, Pieces.Action_Kind_List;
  begin
    case Action.Kind is
      when Pieces.Move =>
        Put ("Move to ");
        Put (Action.Dest);
      when Pieces.Take =>
        Put ("Take at ");
        Put (Action.Dest);
      when Pieces.Cover =>
        Put ("Cover ");
        Put (Action.Dest);
      when Pieces.Take_En_Passant =>
        Put ("Take en passant at ");
        Put (Action.Dest);
      when Pieces.Castle =>
        Put ("Castle with ");
        Put (Action.Rook_From);
      when Pieces.Promote =>
        Put ("Move to ");
        Put (Action.Dest);
        Put (" Promoting to ");
        Put (Pieces.Piece_Kind_List'Image(Action.New_Piece));
      when Pieces.Take_And_Promote =>
        Put ("Take at ");
        Put (Action.Dest);
        Put (" Promoting to ");
        Put (Pieces.Piece_Kind_List'Image(Action.New_Piece));
    end case;
  exception
    when others =>
      Put ("Exception when putting action.to");
  end Put;


  procedure Put (Action : in Players.Action_Rec) is
    use Ada.Text_Io;
  begin
    if not Action.Valid then
      Put ("Invalid Action");
      return;
    end if;
    begin
      Put (Space.Board.Piece_At(Action.From).all);
    exception
      when others =>
        Put ("Exception when getting piece");
    end;
    Put (' ');
    Put (Action.From);
    Put (' ');
    Put (Action.To);
  exception
    when others =>
      Put ("Exception when putting action");
  end Put;

end Debug;

