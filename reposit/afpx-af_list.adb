separate (Afpx)
package body Af_List is

  Status : List_Status_Rec;
  Opened : Boolean := False;

  List_Window : Af_Con_Io.Window;

  -- Reset/Compute status
  procedure Reset;
  procedure Compute (First_Item_Id : in Positive);

  -- Open / Re-open the list window
  procedure Open is
    use Afpx_Typ;
  begin
    -- Check there is a descriptor
    Af_Dscr.Check;
    -- Close previous window
    if Af_Con_Io.Is_Open (List_Window) then
      Af_Con_Io.Close (List_Window);
    end if;
    -- Start at top
    Reset;
    Modified := True;
    -- Check there is a window in the dscr
    if Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
      Af_Con_Io.Open (List_Window,
                   Af_Con_Io.Full2Con(Af_Dscr.Fields(Lfn).Upper_Left),
                   Af_Con_Io.Full2Con(Af_Dscr.Fields(Lfn).Lower_Right));
      Opened := True;
    else
      Opened := False;
    end if;

  end Open;

  procedure Move_At (Id : in Positive) is
  begin
    Line_List.Move_At (Id);
  end Move_At;

  -- Read and move to next if possible
  procedure Get_Current_Item (Item : out Line_Rec; Move : in Boolean) is
  begin
    if Move and then Line_List.Check_Move then
      Line_List.Read (Item);
    else
      Line_List.Read (Item, Line_List_Mng.Current);
    end if;
  end Get_Current_Item;

  procedure Put (Row : in Af_Con_Io.Row_Range; State : in Af_Ptg.State_List;
                 Item : in Line_Rec) is
    Str : Unicode_Sequence (1 .. Af_Dscr.Fields(Lfn).Width)
        := (others => Con_Io.Space);
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Colors;
  begin
    -- Set colors
    Af_Ptg.Set_Colors (Af_Dscr.Fields(Lfn), State,
                       Foreground, Background);
    -- Set str
    if Item.Len > Str'Last then
      Str := Item.Str (Str'Range);
    else
      Str (1 .. Item.Len) := Item.Str (1 .. Item.Len);
    end if;
    -- Move
    Af_Con_Io.Move ( (Row, 0), List_Window);
    -- Put
    Af_Con_Io.Putu (
     S => Str,
     Name => List_Window,
     Foreground => Af_Con_Io.Colors(Foreground),
     Background => Af_Con_Io.Colors(Background),
     Move => False);
  end Put;

  procedure Clear (Row : in Af_Con_Io.Row_Range) is
    Str : constant String (1 .. Af_Dscr.Fields(Lfn).Width) := (others => ' ');
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Colors;
  begin
    -- Set colors
    Af_Ptg.Set_Colors (Af_Dscr.Fields(Lfn), Af_Ptg.Normal,
                       Foreground, Background);
    -- Move
    Af_Con_Io.Move ( (Row, 0), List_Window);
    -- Put
    Af_Con_Io.Put (
     S => Str,
     Name => List_Window,
     Foreground => Af_Con_Io.Colors(Foreground),
     Background => Af_Con_Io.Colors(Background),
     Move => False);
  end Clear;

  procedure Put (Row : in Af_Con_Io.Row_Range;
                 State : in Af_Ptg.State_List;
                 Move : in Boolean) is
    Id : Positive;
    Item : Line_Rec;
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    Id := Status.Id_Top + Row;
    Move_At (Id);
    Get_Current_Item (Item, Move);
    Put (Row, State, Item);
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Put;

  procedure Set_Colors is
  begin
    Af_Con_Io.Set_Foreground (
     Af_Con_Io.Colors(Af_Dscr.Fields(Lfn).Colors.Foreground), List_Window);
    Af_Con_Io.Set_Background (
     Af_Con_Io.Colors(Af_Dscr.Fields(Lfn).Colors.Background), List_Window);
  end Set_Colors;

  -- Reset status
  procedure Reset is
  begin
      Status.Nb_Rows := 0;
      Status.Id_Top := 0;
      Status.Id_Bottom := 0;
      Status.Ids_Selected := (others => 0);
  end Reset;

  -- Compute status
  procedure Compute (First_Item_Id : in Positive) is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Line_List.Is_Empty then
      Reset;
      return;
    end if;

    if      Status.Ids_Selected(List_Left)  > Line_List.List_Length
    or else Status.Ids_Selected(List_Right) > Line_List.List_Length
    then
      raise Line_List_Mng.Not_In_List;
    end if;
    -- top + height - 1 <= length => can display Height items
    if Line_List.List_Length - First_Item_Id >= Af_Dscr.Fields(Lfn).Height then
      -- Can display Height items
      Status.Nb_Rows := Af_Dscr.Fields(Lfn).Height;
      Status.Id_Top := First_Item_Id;
    elsif Line_List.List_Length < Af_Dscr.Fields(Lfn).Height then
      -- Cannot display List_Length items whatever first
      Status.Nb_Rows := Line_List.List_Length;
      Status.Id_Top := 1;
    else
      -- Can display Height items but not with this first.
      -- Set top to display last page
      Status.Nb_Rows := Af_Dscr.Fields(Lfn).Height;
      Status.Id_Top := Line_List.List_Length - Af_Dscr.Fields(Lfn).Height + 1;
    end if;
    Status.Id_Bottom := Status.Id_Top + Status.Nb_Rows - 1;
    -- Left select by default the first
    if Status.Ids_Selected(List_Left) = 0 then
      Status.Ids_Selected(List_Left) := Status.Id_Top;
    end if;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Compute;

  -- Display the list, starting from First_Item
  procedure Display (First_Item_Id : in Positive) is
    Item : Line_Rec;
    List_Pos : Positive;
    List_Mod : Boolean;
  begin
    -- Set status
    Compute (First_Item_Id);
    Modified := False;

    if Line_List.Is_Empty then
      Set_Colors;
      Af_Con_Io.Clear (List_Window);
      return;
    end if;

    -- Save position and list status
    List_Mod := Line_List.Is_Modified;
    List_Pos := Line_List.Get_Position;

    -- Display list
    Move_At (Status.Id_Top);
    for I in 1 .. Status.Nb_Rows loop
      Get_Current_Item (Item, True);
      if not Af_Dscr.Fields(Lfn).Isprotected
      and then Status.Id_Top + I - 1 = Status.Ids_Selected(List_Left) then
        Put (I - 1, Af_Ptg.Selected, Item);
      elsif not Af_Dscr.Fields(Lfn).Isprotected
      and then Status.Id_Top + I - 1 = Status.Ids_Selected(List_Right) then
        Put (I - 1, Af_Ptg.Clicked, Item);
      else
        Put (I - 1, Af_Ptg.Normal, Item);
      end if;
    end loop;

    -- Restore Position and list status
    Move_At (List_Pos);
    if not List_Mod then
      -- Not seen... not caught
      Line_List.Modification_Ack;
    end if;

    -- Display empty end of list (if any)
    for I in Status.Nb_Rows + 1 .. Af_Dscr.Fields(Lfn).Height loop
      Clear (I - 1);
    end loop;

  exception
    when others =>
      raise Afpx_Internal_Error;
  end Display;

  -- Actions on the list
  -- type Action_List is (Up, Down, Page_Up, Page_Down...);

  -- Update the list due to an action, display the list or not
  function Update (Action : in List_Action_List; Display : in Boolean)
                  return Boolean is
    First_Item_Id : Natural;
    Shift_Factor : constant := 10;
    Height : constant Positive := Af_Dscr.Fields(Lfn).Height;
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    -- Update may be called before 1st Ptg
    if Status.Id_Top = 0 then
      Compute (1);
    end if;
    -- List is empty
    if Line_List.Is_Empty then
      return False;
    end if;

    -- Update selection, cause current may have changed
    -- called by user
    Set_Selected (List_Left, Line_List.Get_Position);

    -- Recompute cause list may have changed
    Compute (Status.Id_Top);

    -- Nothing to scroll
    if Status.Nb_Rows /= Height then
      return False;
    end if;

    case Action is
      when Up =>
        -- Scroll 1 row down
        if Status.Id_Top = 1 then
          return False;
        end if;
        First_Item_Id := Status.Id_Top - 1;
      when Down =>
        -- Scroll 1 row down
        if Status.Id_Bottom = Line_List.List_Length then
          return False;
        end if;
        First_Item_Id := Status.Id_Top + 1;
      when Page_Up =>
        -- Display previous page
        -- top - height > 1 => top - height exists
        if Status.Id_Top > Height + 1 then
          First_Item_Id := Status.Id_Top - Height;
        elsif Status.Id_Top /= 1 then
          -- Start at first item
          First_Item_Id := 1;
        else
          -- Already at top of list
          return False;
        end if;
      when Page_Down =>
        -- Display next page
        -- Bottom + height < length => Bottom + height exists
        if Line_List.List_Length - Status.Id_Bottom > Height then
          First_Item_Id := Status.Id_Top + Height;
        elsif Status.Id_Bottom /= Line_List.List_Length then
          -- End at last item
          First_Item_Id := Line_List.List_Length - Height + 1;
        else
          -- Already at bottom of list
          return False;
        end if;
      when Shift_Page_Up =>
        -- Display previous page
        -- top - height > 1 => top - height exists
        if Status.Id_Top > Shift_Factor * Height + 1 then
          First_Item_Id := Status.Id_Top - Shift_Factor * Height;
        elsif Status.Id_Top /= 1 then
          -- Start at first item
          First_Item_Id := 1;
        else
          -- Already at top of list
          return False;
        end if;
      when Shift_Page_Down =>
        -- Display next page
        -- Bottom + height < length => Bottom + height exists
        if Line_List.List_Length - Status.Id_Bottom > Shift_Factor * Height then
          First_Item_Id := Status.Id_Top + Shift_Factor * Height;
        elsif Status.Id_Bottom /= Line_List.List_Length then
          -- End at last item
          First_Item_Id := Line_List.List_Length - Height + 1;
        else
          -- Already at bottom of list
          return False;
        end if;
      when Top =>
        -- Move to top of list
        if Status.Id_Top = 1 then
          -- Already at top of list
          return False;
        end if;
        First_Item_Id := 1;
      when Bottom =>
        -- Move to bottom of list
        if Status.Id_Bottom = Line_List.List_Length then
          -- Already at bottom of list
          return False;
        end if;
        First_Item_Id := Line_List.List_Length - Height + 1;
      when Center =>
        -- Center current List item in window (do ower best)
        declare
          -- List length
          Len : constant Positive := Line_List.List_Length;
          -- Current position in list
          Pos : constant Positive := Line_List.Get_Position;
          -- Row in window to put it
          Midrow : constant Natural := Height / 2;
          Lastrow : constant Natural := Height - 1;
        begin
          if Pos - 1 < Midrow then
            return Update(Top, Display);
          elsif Len - Pos < Lastrow - Midrow then
            return Update(Bottom, Display);
          else
            First_Item_Id := Pos - Midrow;
          end if;
        end;
    end case;

    -- Display or just re-compute and tag
    if Display then
      -- Redisplay the list (called by PtG)
      Af_List.Display (First_Item_Id);
    else
      -- Recompute and mark modified for nexwt PtG (called by client)
      Compute (First_Item_Id);
      Modified := True;
    end if;

    return True;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Update;

  -- Set the current item (selected_color) of the lis
  procedure Set_Selected (Button : in List_Button_List; Item_Id : in Natural) is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Line_List.Is_Empty then
      if Item_Id /= 0 then
        raise Line_List_Mng.Not_In_List;
      end if;
    elsif Item_Id > Line_List.List_Length
    or else (Button = List_Left and then Item_Id = 0) then
      raise Line_List_Mng.Not_In_List;
    end if;
    if Status.Ids_Selected(Button) /= Item_Id then
      Status.Ids_Selected(Button) := Item_Id;
      Modified := True;
    end if;
  end Set_Selected;

  -- Status of the list
  function Get_Status return List_Status_Rec is
  begin
    -- Update may be called before 1st Ptg
    if Af_Dscr.Has_List and then Status.Id_Top = 0 then
      Compute (1);
    end if;
    return Status;
  end Get_Status;

  -- Percent of position of list in list field
  function Get_Percent return Percent_Range is
    Last_Top : Integer;
    Height : constant Positive := Af_Dscr.Fields(Lfn).Height;
  begin
    if not Af_Dscr.Has_List then
      -- No list field
      return 0;
    elsif Line_List.List_Length <= Height then
      -- List shorter than field
      return 0;
    end if;

    -- At which percent is the bottom shown
    -- Top index when at bottom
    Last_Top := Line_List.List_Length - Height + 1;
    -- Factor = (100 - 1) / (LastTop - 1)
    -- Percent - 1 = (Top - 1) * Factor
    return (Get_Status.Id_Top - 1) * (100 - 1) / (Last_Top - 1)  + 1;
  end Get_Percent;

  procedure Set_Current is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Line_List.Is_Empty then
      return;
    end if;
    Move_At (Status.Ids_Selected(List_Left));
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Set_Current;

  -- Is an Id, a row displayed
  function Id_Displayed (Id : Positive) return Boolean is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    return Id >= Status.Id_Top and then Id <= Status.Id_Bottom;
  end Id_Displayed;

  function Row_Displayed (Row : Af_Con_Io.Row_Range) return Boolean is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    return Row < Status.Nb_Rows;
  end Row_Displayed;

  -- Row <-> Item Id
  function To_Row (Id : Positive) return Af_Con_Io.Row_Range is
  begin
    if not Id_Displayed (Id) then
      raise Afpx_Internal_Error;
    end if;
    return Id - Status.Id_Top;
  end To_Row;

  function To_Id  (Row : Af_Con_Io.Row_Range) return Positive is
  begin
    if not Row_Displayed (Row) then
      raise Afpx_Internal_Error;
    end if;
    return Row + Status.Id_Top;
  end To_Id;

end Af_List;

