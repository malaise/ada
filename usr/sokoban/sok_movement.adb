with Sok_Display;

-- Movement manager of SOKOBAN
package body Sok_Movement is


  function New_Coordinate (
   Position : Sok_Types.Coordinate_Rec;
   Movement : Movement_List) return Sok_Types.Coordinate_Rec is
    New_Position : Sok_Types.Coordinate_Rec := Position;
  begin
    case Movement is
      when Sok_Input.Up =>
        New_Position.Row :=
         Sok_Types.Row_Range'Pred(New_Position.Row);
      when Sok_Input.Down =>
        New_Position.Row :=
         Sok_Types.Row_Range'Succ(New_Position.Row);
      when Sok_Input.Left =>
        New_Position.Col :=
         Sok_Types.Col_Range'Pred(New_Position.Col);
      when Sok_Input.Right =>
        New_Position.Col :=
         Sok_Types.Col_Range'Succ(New_Position.Col);
    end case;
    return New_Position;
  exception
    when others => raise Illegal_Movement;
  end New_Coordinate;



  -- try to do a movement
  -- Give frame, current position and movement to try
  procedure Do_Movement (
   Frame    : in out Sok_Types.Frame_Tab;
   Position : in out Sok_Types.Coordinate_Rec;
   Movement : in Movement_List;
   Result   : out Result_List) is

   Cur_Man_Square   : Sok_Types.Square_Rec;

   New_Man_Position : Sok_Types.Coordinate_Rec;
   New_Man_Square   : Sok_Types.Square_Rec;

   New_Box_Position : Sok_Types.Coordinate_Rec;
   New_Box_Square   : Sok_Types.Square_Rec;

   Loc_Result : Result_List;

   use Sok_Types;
  begin
    -- check content of current position
    Cur_Man_Square := Frame (Position.Row, Position.Col);
    if Cur_Man_Square.Pattern = Sok_Types.Wall or else
     Cur_Man_Square.Content /= Sok_Types.Man then
      -- on a wall or no man !!
      raise Illegal_Movement;
    end if;

    -- set new position
    New_Man_Position := New_Coordinate (Position, Movement);
    New_Man_Square := Frame (New_Man_Position.Row, New_Man_Position.Col);

    -- check content of new man position
    case New_Man_Square.Pattern is
      when Sok_Types.Wall =>
        Loc_Result := Refused;

      when Sok_Types.Free | Sok_Types.Target =>
        case New_Man_Square.Content is
          when Sok_Types.Man =>
            -- another man !!
            raise Illegal_Movement;
          when Sok_Types.Nothing =>
            -- simple man movement
            Loc_Result := Done;
          when Sok_Types.Box =>
            -- a push
            New_Box_Position := New_Coordinate (New_Man_Position, Movement);
            New_Box_Square :=
             Frame (New_Box_Position.Row, New_Box_Position.Col);

            -- check content of new box position
            case New_Box_Square.Pattern is
              when Sok_Types.Wall =>
                Loc_Result := Refused;

              when Sok_Types.Free | Sok_Types.Target =>
                case New_Box_Square.Content is
                  when Sok_Types.Man =>
                    -- another man !!
                    raise Illegal_Movement;
                  when Sok_Types.Box =>
                    -- impossible to push 2 boxes
                    Loc_Result := Refused;
                  when Sok_Types.Nothing =>
                    -- push
                    if New_Man_Square.Pattern = New_Box_Square.Pattern then
                      -- a push from free to free or from target to target
                      Loc_Result := Box_Moved;
                    elsif New_Man_Square.Pattern = Sok_Types.Free then
                      -- a push from free to target
                      Loc_Result := Box_Ok_More;
                    else
                      -- a push from target to free
                      Loc_Result := Box_Ok_Less;
                    end if;
                end case;
            end case;
        end case;
    end case;


    -- update old and new man position
    case Loc_Result is
      when Refused =>
        null;
      when Done | Box_Moved | Box_Ok_More | Box_Ok_Less =>
        Frame (Position.Row, Position.Col).Content := Sok_Types.Nothing;
        Sok_Display.Put_Square (
         Square     => Frame (Position.Row, Position.Col),
         Coordinate => Position);
        Position := New_Man_Position;

        Frame (New_Man_Position.Row, New_Man_Position.Col).Content :=
         Sok_Types.Man;
        Sok_Display.Put_Square (
         Square     => Frame (New_Man_Position.Row, New_Man_Position.Col),
         Coordinate => New_Man_Position);
    end case;

    -- update new box position
    case Loc_Result is
      when Refused | Done =>
        null;
      when Box_Moved | Box_Ok_More | Box_Ok_Less =>
        Frame (New_Box_Position.Row, New_Box_Position.Col).Content :=
         Sok_Types.Box;
        Sok_Display.Put_Square (
         Square     => Frame (New_Box_Position.Row, New_Box_Position.Col),
         Coordinate => New_Box_Position);
    end case;

    -- Done
    Result := Loc_Result;
  end Do_Movement;



  -- to undo a movement
  -- give frame, current position and movement which
  -- moved to current position
  procedure Undo_Movement (
   Frame      : in out Sok_Types.Frame_Tab;
   Saved_Data : in Saved_Data_Rec;
   Result        : out Undo_Result_List;
   Prev_Position : out Sok_Types.Coordinate_Rec) is

    Cur_Man_Square   : Sok_Types.Square_Rec;

    Old_Man_Position : Sok_Types.Coordinate_Rec;
    Old_Man_Square   : Sok_Types.Square_Rec;

    Cur_Box_Position : Sok_Types.Coordinate_Rec;
    Cur_Box_Square   : Sok_Types.Square_Rec;

    Loc_Result : Undo_Result_List;

    Undo_Movement : Movement_List;

    use Sok_Types;
  begin
    -- check content of current position
    Cur_Man_Square := Frame (Saved_Data.Pos_Orig.Row,
                             Saved_Data.Pos_Orig.Col);
    if Cur_Man_Square.Pattern = Sok_Types.Wall or else
     Cur_Man_Square.Content /= Sok_Types.Man then
      -- on a wall or no man !!
      raise Illegal_Movement;
    end if;

    -- set undo movement
    case Saved_Data.Movement is
      when Sok_Input.Up    => Undo_Movement := Sok_Input.Down;
      when Sok_Input.Down  => Undo_Movement := Sok_Input.Up;
      when Sok_Input.Right => Undo_Movement := Sok_Input.Left;
      when Sok_Input.Left  => Undo_Movement := Sok_Input.Right;
    end case;

    -- set previous man position
    Old_Man_Position := New_Coordinate (Saved_Data.Pos_Orig, Undo_Movement);
    Old_Man_Square := Frame (Old_Man_Position.Row, Old_Man_Position.Col);

    -- check content of old man position
    case Old_Man_Square.Pattern is
      when Sok_Types.Wall =>
        raise Illegal_Movement;

      when Sok_Types.Free | Sok_Types.Target =>
        case Old_Man_Square.Content is
          when Sok_Types.Man | Sok_Types.Box =>
            -- another man, or a box at old man position !!
            raise Illegal_Movement;
          when Sok_Types.Nothing =>
            -- OK check if also box has to be moved
            case Saved_Data.Result is
              when Done =>
                Loc_Result := Done;
              when Box_Moved =>
                -- set current box position
                Cur_Box_Position := New_Coordinate (Saved_Data.Pos_Orig,
                                                    Saved_Data.Movement);
                Cur_Box_Square := Frame (Cur_Box_Position.Row,
                                         Cur_Box_Position.Col);

                -- check content of current box position
                case Cur_Box_Square.Pattern is
                  when Sok_Types.Wall =>
                    raise Illegal_Movement;

                  when Sok_Types.Free | Sok_Types.Target =>
                    case Cur_Box_Square.Content is
                      when Sok_Types.Man | Sok_Types.Nothing =>
                        -- another man or nothing !!
                        raise Illegal_Movement;
                      when Sok_Types.Box =>
                        -- OK, pop box
                        if Cur_Man_Square.Pattern = Cur_Box_Square.Pattern then
                          -- a pop from free to free or from target to target
                          Loc_Result := Box_Moved;
                        elsif Cur_Man_Square.Pattern = Sok_Types.Free then
                          -- a pop from target to free
                          Loc_Result := Box_Ok_Less;
                        else
                          -- a pop from free to target
                          Loc_Result := Box_Ok_More;
                        end if;
                    end case;
                end case;
            end case;
        end case;
    end case;

    -- update old box position, and position content
    case Loc_Result is
      when Done =>
        -- original man becomes empty
        Frame (Saved_Data.Pos_Orig.Row, Saved_Data.Pos_Orig.Col).Content :=
               Sok_Types.Nothing;
      when Box_Moved | Box_Ok_More | Box_Ok_Less =>
        -- original man position receives box
        Frame (Saved_Data.Pos_Orig.Row, Saved_Data.Pos_Orig.Col).Content :=
               Sok_Types.Box;
        -- original box posoition becomes empty. Show
        Frame (Cur_Box_Position.Row, Cur_Box_Position.Col).Content :=
               Sok_Types.Nothing;
        Sok_Display.Put_Square (
         Square     => Frame (Cur_Box_Position.Row, Cur_Box_Position.Col),
         Coordinate => Cur_Box_Position);
    end case;


    -- update current and old man position
    -- show original man position
    Sok_Display.Put_Square (
     Square     => Frame (Saved_Data.Pos_Orig.Row, Saved_Data.Pos_Orig.Col),
     Coordinate => Saved_Data.Pos_Orig);
    -- set and show current man position
    Frame (Old_Man_Position.Row, Old_Man_Position.Col).Content :=
     Sok_Types.Man;
    Sok_Display.Put_Square (
     Square     => Frame (Old_Man_Position.Row, Old_Man_Position.Col),
     Coordinate => Old_Man_Position);

    -- Done
    Prev_Position := Old_Man_Position;
    Result := Loc_Result;
 end Undo_Movement;

end Sok_Movement;