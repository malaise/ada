with Ada.Text_Io;
with Dynamic_List;

with Team, Space.Board, Debug;

package body Players is

  subtype True_Action is Action_Rec(True);
  package Action_List_Mng is new Dynamic_List (True_Action);
  Actions : array (Space.Color_List) of Action_List_Mng.List_Type;
  At_End : array (Space.Color_List) of Boolean := (others => True);


  -- Build list of possible movements
  procedure Think (Color : in Space.Color_List) is
    Piece : Pieces.Piece_Access;
    use type Pieces.Piece_Access;
    Player_Board_Error : exception;
  begin
    if Debug.Get (Debug.Think) then
      Ada.Text_Io.Put_Line ("Player " & Space.Color_List'Image(Color)
                          & " is thinking");
    end if;
    -- Clear list of movements
    Action_List_Mng.Delete_List (Actions(Color), Deallocate => True);
    -- For each piece of team
    Team.Rewind (Color);

    One_Piece :
    loop
      Piece := Team.Get (Color);
      exit when Piece = null;
      declare
        Pos : constant Space.Square_Coordinate
            := Pieces.Pos_Of (Piece.all);
        Arr : constant Pieces.Action_Array
            := Pieces.Actions_Of (Piece.all);
      begin
        -- Sanity
        if Space.Board.Piece_At (Pos) /= Piece then
          raise Player_Board_Error;
        end if;
        for I in Arr'Range loop
          Action_List_Mng.Insert (Actions(Color), (True, Pos, Arr(I)) );
          if Debug.Get (Debug.Think) then
            Ada.Text_Io.Put ("-> ");
            Debug.Put (True_Action'(True, Pos, Arr(I)));
            Ada.Text_Io.New_Line;
          end if;
        end loop;
      end;
    end loop One_Piece;

    At_End (Color) := True;
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
    Result : True_Action;
  begin
    if Action_List_Mng.Is_Empty (Actions(Color)) or else At_End(Color) then
      return (Valid => False);
    end if;
    if Action_List_Mng.Get_Position (Actions(Color))
     = Action_List_Mng.list_Length(Actions(Color)) then
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Current);
      At_End(Color) := True;
    else
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Next);
    end if;
    return Result;
  end Next_Action;

  function Get_Action (Color : Space.Color_List; Index : Positive) return Action_Rec is
    Result : True_Action;
    Pos : Positive;
  begin
    if Action_List_Mng.Is_Empty (Actions(Color)) then
      return (Valid => False);
    end if;
    Pos := Action_List_Mng.Get_Position (Actions(Color));

    begin
      Action_List_Mng.Move_To (Actions(Color), Number => Index - 1, From_current => False);
      Action_List_Mng.Read (Actions(Color), Result, Action_List_Mng.Current);
    exception
      when Action_List_Mng.Not_In_List =>
        return (Valid => False);
    end;
    Action_List_Mng.Move_To (Actions(Color), Number => Pos - 1, From_current => False);
    return Result;
  end Get_Action;

  
  function Match (Action, Ref : True_Action) return Boolean is
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

  procedure Search_Action is new Action_List_Mng.Search (Match); 

  function Find_Action (Color : Space.Color_List;
                        From, To : Space.Square_Coordinate;
                        Promote  : in Pieces.Piece_Kind_List) return Action_Rec is
    Ref : True_Action; 
    Res : True_Action;
  begin
    if Promote in Pieces.Promotion_Piece_List then
      Ref := (Valid => True,
              From  => From,
              To    => (Pieces.Promote, Dest => To, New_Piece => Promote) );
    else
      Ref := (Valid => True,
              From  => From,
              To    => (Pieces.Move, Dest => To) );
    end if;

    -- Search first occurence
    begin
      Search_Action(Actions(Color), Ref, From_Current => False);
    exception
       when Action_List_Mng.Not_In_List =>
         -- Not found
         return (Valid => False);
    end;

    -- Got one
    Action_List_Mng.Read(Actions(Color), Res, Action_List_Mng.Current);

    begin
      Action_List_Mng.Move_To(Actions(Color));
      Search_Action(Actions(Color), Ref, From_Current => True);
      -- Should be unique
      raise More_Than_One;
    exception
      when Action_List_Mng.Not_In_List =>
      return Res;
    end;
  end Find_Action;

end Players;

