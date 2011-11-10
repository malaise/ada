with Lower_Str, Normal, Basic_Proc;
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
    Basic_Proc.Put_Output (Lower_Str(Space.Col_Range'Image(Square.Col))
                   & Normal(Integer(Square.Row), 1) );
  exception
    when others =>
      Basic_Proc.Put_Output ("Exception when putting square");
  end Put;

  procedure Put (Piece : in Pieces.Basic_Piece'Class) is
    Id : constant Pieces.Piece_Id := Pieces.Id_Of (Piece);
  begin
    Basic_Proc.Put_Output (Space.Color_List'Image(Id.Color)
         & " " & Pieces.Piece_Kind_List'Image(Id.Kind));
  exception
    when others =>
      Basic_Proc.Put_Output ("Exception when putting piece");
  end Put;

  procedure Put (Action : in Pieces.Action_Rec) is
    use type Space.Square_Coordinate, Pieces.Action_Kind_List;
  begin
    case Action.Kind is
      when Pieces.Move =>
        Basic_Proc.Put_Output ("Move to ");
        Put (Action.Dest);
      when Pieces.Take =>
        Basic_Proc.Put_Output ("Take at ");
        Put (Action.Dest);
      when Pieces.Cover =>
        Basic_Proc.Put_Output ("Cover ");
        Put (Action.Dest);
      when Pieces.Take_En_Passant =>
        Basic_Proc.Put_Output ("Take en passant at ");
        Put (Action.Dest);
      when Pieces.Castle =>
        Basic_Proc.Put_Output ("Castle with ");
        Put (Action.Rook_From);
      when Pieces.Promote =>
        Basic_Proc.Put_Output ("Move to ");
        Put (Action.Dest);
        Basic_Proc.Put_Output (" Promoting to ");
        Basic_Proc.Put_Output (Action.New_Piece'Img);
      when Pieces.Take_And_Promote =>
        Basic_Proc.Put_Output ("Take at ");
        Put (Action.Dest);
        Basic_Proc.Put_Output (" Promoting to ");
        Basic_Proc.Put_Output (Pieces.Piece_Kind_List'Image(Action.New_Piece));
    end case;
  exception
    when others =>
      Basic_Proc.Put_Output ("Exception when putting action.to");
  end Put;


  procedure Put (Action : in Players.Action_Rec) is
  begin
    if not Action.Valid then
      Basic_Proc.Put_Output ("Invalid Action");
      return;
    end if;
    begin
      Put (Space.Board.Piece_At(Action.From).all);
    exception
      when others =>
        Basic_Proc.Put_Output ("Exception when getting piece");
    end;
    Basic_Proc.Put_Output (' ' & "");
    Put (Action.From);
    Basic_Proc.Put_Output (' ' & "");
    Put (Action.To);
  exception
    when others =>
      Basic_Proc.Put_Output ("Exception when putting action");
  end Put;

end Debug;

