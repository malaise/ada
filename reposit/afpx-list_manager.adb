package body Afpx.List_Manager is

  -- Scroll the list according to button
  -- Assumption is that buttons are consecutive in the order:
  -- Top(0), PgUp(1), Up(2), Center(3), Down(4), PgDown(5), Bottom(6)
  -- subtype Offset_Range is Field_Range range 1 .. 6;
  procedure Scroll (Offset : in Offset_Range) is
  begin
    case Offset is
      when 1 => Update_List(Top);
      when 2 => Update_List(Page_Up);
      when 3 => Update_List(Up);
      when 4 => Update_List(Center);
      when 5 => Update_List(Down);
      when 6 => Update_List(Page_Down);
      when 7 => Update_List(Bottom);
    end case;
  end Scroll;

  -- Initialize Afpx list from From list
  procedure Init_List (From : in out Element_List.Dyn_List.List_Type) is
    Pos : Positive;
    Elt : Element_Type;
    Done : Boolean;
    Line : Line_Rec;
  begin
    -- Delete Afpx list
    Line_List.Delete_List;
    -- Done if empty list
    if From.Is_Empty then
      return;
    end if;
    -- Save position in list
    Pos := From.Get_Position;

    -- Copy list
    From.Rewind;
    loop
      From.Read (Elt, Done => Done);
      Set (Line, Elt);
      Line_List.Insert (Line);
      exit when not Done;
    end loop;

    -- Set both lists at pos
    From.Move_At (Pos);
    Line_List.Move_At (Pos);
  end Init_List;

end Afpx.List_Manager;

