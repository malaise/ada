with Ada.Text_Io;
with Dynamic_List;

with Team, Space.Board, Debug;

package body Players is

  package Action_List_Mng is new Dynamic_List (Valid_Action_Rec);
  Actions : array (Space.Color_List) of Action_List_Mng.List_Type;
  At_End : array (Space.Color_List) of Boolean := (others => True);


  -- Build list of possible movements
  procedure Think (Color : in Space.Color_List) is
    Piece : Pieces.Piece_Access;
    use type Pieces.Piece_Access, Pieces.Piece_Kind_List;
    Player_Board_Error : exception;
    King_Found : Boolean;
  begin
    if Debug.Get (Debug.Think) then
      Ada.Text_Io.Put_Line ("Player " & Space.Color_List'Image(Color)
                          & " is thinking");
    end if;
    -- Clear list of movements
    Action_List_Mng.Delete_List (Actions(Color), Deallocate => True);
    -- For each piece of team
    Team.Rewind (Color);
    King_Found := False;

    One_Piece :
    loop
      Piece := Team.Get (Color);
      exit when Piece = null;
      declare
        Pos : constant Space.Square_Coordinate
            := Pieces.Pos_Of (Piece.all);
        Arr : constant Pieces.Action_Array
            := Pieces.Actions_Of (Piece.all);
        Piece_Kind : constant Pieces.Piece_Kind_List
                   := Pieces.Id_Of (Piece.all).Kind;
      begin
        -- Sanity
        if Space.Board.Piece_At (Pos) /= Piece then
          raise Player_Board_Error;
        end if;
        if Piece_Kind = Pieces.King then
          King_Found := True;
        end if;
        for I in Arr'Range loop
          Action_List_Mng.Insert (Actions(Color), (True, Piece_Kind, Pos, Arr(I)) );
          if Debug.Get (Debug.Think) then
            Ada.Text_Io.Put ("-> ");
            Debug.Put (Valid_Action_Rec'(True, Piece_Kind, Pos, Arr(I)));
            Ada.Text_Io.New_Line;
          end if;
        end loop;
      end;
    end loop One_Piece;

    At_End (Color) := True;
    if not King_Found then
      raise Player_Board_Error;
    end if;
    if Debug.Get (Debug.Think) then
      Ada.Text_Io.Put_Line ("Player " & Space.Color_List'Image(Color)
                          & " has finished thinking");
    end if;
  end Think;

  -- Returns a not Valid movement when end of list
  procedure Rewind_Actions (Color : in Space.Color_List) is
  begin
    if not Action_List_Mng.Is_Empty (Actions(Color)) then
      Action_List_Mng.Move_To (Actions(Color), Action_List_Mng.Next, 0, False);
    end if;
    At_End (Color) := False;
  end Rewind_Actions;

  function Next_Action (Color : Space.Color_List) return Action_Rec is
    Result : Valid_Action_Rec;
  begin
    if Action_List_Mng.Is_Empty (Actions(Color)) or else At_End(Color) then
      return (Valid => False);
    end if;
    if Action_List_Mng.Get_Position (Actions(Color))
     = Action_List_Mng.List_Length(Actions(Color)) then
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Current);
      At_End(Color) := True;
    else
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Next);
    end if;
    return Result;
  end Next_Action;

  function Get_Action (Color : Space.Color_List; Index : Positive) return Action_Rec is
    Result : Valid_Action_Rec;
    Pos : Positive;
  begin
    if Action_List_Mng.Is_Empty (Actions(Color)) then
      return (Valid => False);
    end if;
    Pos := Action_List_Mng.Get_Position (Actions(Color));

    begin
      Action_List_Mng.Move_To (Actions(Color), Number => Index - 1, From_Current => False);
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Current);
    exception
      when Action_List_Mng.Not_In_List =>
        return (Valid => False);
    end;
    Action_List_Mng.Move_To (Actions(Color), Number => Pos - 1, From_Current => False);
    return Result;
  end Get_Action;

  
  function Match (Action, Ref : Valid_Action_Rec) return Boolean is
    use type Pieces.Action_Kind_List, Pieces.Piece_Kind_List,
             Space.Square_Coordinate;
  begin
    if Action.To.Kind = Pieces.Cover then
      return False;
    end if;
    if Ref.To.Kind /= Pieces.Promote then
      return Action.To.Kind /= Pieces.Promote
        and then Action.To.Kind /= Pieces.Take_And_Promote
        and then Action.From = Ref.From
        and then Action.To.Dest = Ref.To.Dest;
    else
      return (Action.To.Kind = Pieces.Promote
              or else Action.To.Kind = Pieces.Take_And_Promote)
        and then Action.From = Ref.From
        and then Action.To.Dest = Ref.To.Dest
        and then Action.To.New_Piece = Ref.To.New_Piece;
    end if;
  end Match;

  procedure Search_Match_Action is new Action_List_Mng.Search (Match); 

  function Find_Action (Color : Space.Color_List;
                        From, To : Space.Square_Coordinate;
                        Promote  : in Pieces.Piece_Kind_List) return Action_Rec is
    Ref : Valid_Action_Rec; 
    Res : Valid_Action_Rec;
  begin
    if Promote in Pieces.Promotion_Piece_List then
      Ref := (Valid => True,
              Piece => Pieces.Pawn,
              From  => From,
              To    => (Pieces.Promote, Dest => To, New_Piece => Promote) );
    else
      Ref := (Valid => True,
              Piece => Pieces.Pawn,
              From  => From,
              To    => (Pieces.Move, Dest => To) );
    end if;

    -- Search first occurence
    begin
      Search_Match_Action(Actions(Color), Ref,
                          From => Action_List_Mng.Absolute);
    exception
       when Action_List_Mng.Not_In_List =>
         -- Not found
         return (Valid => False);
    end;

    -- Got one
    Action_List_Mng.Read(Actions(Color), Res, Action_List_Mng.Current);

    begin
      Action_List_Mng.Move_To(Actions(Color));
      Search_Match_Action(Actions(Color), Ref,
                          From => Action_List_Mng.From_Current);
      -- Should be unique
      raise More_Than_One;
    exception
      when Action_List_Mng.Not_In_List =>
      return Res;
    end;
  end Find_Action;

  -- Check an action exists the the list
  function Same (Action, Ref : Valid_Action_Rec) return Boolean is
  begin
    return Action = Ref;
  end Same;

  procedure Search_Same_Action is new Action_List_Mng.Search (Same); 

  function Action_Exists (Color : Space.Color_List; Action : Valid_Action_Rec) return Boolean is
  begin
    Search_Same_Action (Actions(Color), Action,
                        From => Action_List_Mng.Absolute);
    return True;
  exception
    when Action_List_Mng.Not_In_List =>
       -- Not found
       return False;
  end Action_Exists;

end Players;

