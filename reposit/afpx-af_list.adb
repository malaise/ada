separate (Afpx)
package body Af_List is

  Status : Status_Rec;
  Opened : Boolean := False;

  List_Window : Con_Io.Window;

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
    if Con_Io.Is_Open (List_Window) then
      Con_Io.Close (List_Window);
    end if;
    -- Check there is a window in the dscr
    if Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
      Con_Io.Open (List_Window,
                   Af_Dscr.Fields(Lfn).Upper_Left,
                   Af_Dscr.Fields(Lfn).Lower_Right);
      Opened := True;
      -- Start at top
      Status.Id_Selected := 0;
      Reset;
    else
      Opened := False;
    end if;

  end Open;

  procedure Move (Id : in Positive) is
  begin
    Line_List_Mng.Move_To (Line_List, Line_List_Mng.Next, Id - 1, False);
  end Move;

  procedure Get_Current_Item (Item : out Line_Rec) is
  begin
    Line_List_Mng.Read (Line_List, Item, Line_List_Mng.Next);
  exception
    when Line_List_Mng.Not_In_List =>
      Line_List_Mng.Read (Line_List, Item, Line_List_Mng.Current);
  end Get_Current_Item;

  procedure Put (Row : in Con_Io.Row_Range; State : in Af_Ptg.State_List;
                 Item : in Line_Rec) is
    Str : String (1 .. Af_Dscr.Fields(Lfn).Width) := (others => ' ');
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Basic_Colors;
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
    Con_Io.Move ( (Row, 0), List_Window);
    -- Put
    Con_Io.Put (S => Str,
                Name => List_Window,
                Foreground => Foreground,
                Blink_Stat => Af_Dscr.Fields(0).Colors.Blink_Stat,
                Background => Background,
                Move => False);
  end Put;

  procedure Clear (Row : in Con_Io.Row_Range) is
    Str : constant String (1 .. Af_Dscr.Fields(Lfn).Width) := (others => ' ');
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Basic_Colors;
  begin
    -- Set colors
    Af_Ptg.Set_Colors (Af_Dscr.Fields(Lfn), Af_Ptg.Normal,
                       Foreground, Background);
    -- Move
    Con_Io.Move ( (Row, 0), List_Window);
    -- Put
    Con_Io.Put (S => Str,
                Name => List_Window,
                Foreground => Foreground,
                Blink_Stat => Af_Dscr.Fields(Lfn).Colors.Blink_Stat,
                Background => Background,
                Move => False);
  end Clear;

  procedure Put (Row : in Con_Io.Row_Range; State : in Af_Ptg.State_List) is
    Id : Positive;
    Item : Line_Rec;
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    Id := Status.Id_Top + Row;
    Move (Id);
    Get_Current_Item (Item);
    Put (Row, State, Item);
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Put;


  procedure Set_Colors is
  begin
    Con_Io.Set_Foreground (Af_Dscr.Fields(Lfn).Colors.Foreground,
                           Af_Dscr.Fields(Lfn).Colors.Blink_Stat, List_Window);
    Con_Io.Set_Background (Af_Dscr.Fields(Lfn).Colors.Background, List_Window);
  end Set_Colors;

  -- Reset status
  procedure Reset is
  begin
      Status.Nb_Rows := 0;
      Status.Id_Top := 0;
      Status.Id_Bottom := 0;
      Status.Id_Selected := 0;
  end Reset;

  -- Compute status
  procedure Compute (First_Item_Id : in Positive) is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Line_List_Mng.Is_Empty (Line_List) then
      Reset;
      return;
    end if;

    if Status.Id_Selected > Line_List_Mng.List_Length (Line_List) then
      raise Line_List_Mng.Not_In_List;
    end if;
    -- top + height - 1 <= length => can display HEIGHT items
    if Line_List_Mng.List_Length (Line_List) - First_Item_Id >=
       Af_Dscr.Fields(Lfn).Height then
      -- Can display HEIGHT items
      Status.Nb_Rows := Af_Dscr.Fields(Lfn).Height;
      Status.Id_Top := First_Item_Id;
    elsif Line_List_Mng.List_Length (Line_List) <
          Af_Dscr.Fields(Lfn).Height then
      -- Cannot display LIST length items whatever first
      Status.Nb_Rows := Line_List_Mng.List_Length (Line_List);
      Status.Id_Top := 1;
    else
      -- Can display HEIGHT items but not with this first.
      -- Set top to display last page
      Status.Nb_Rows := Af_Dscr.Fields(Lfn).Height;
      Status.Id_Top := Line_List_Mng.List_Length (Line_List)
                     - Af_Dscr.Fields(Lfn).Height + 1;
    end if;
    Status.Id_Bottom := Status.Id_Top + Status.Nb_Rows - 1;
    -- Select by default
    if Status.Id_Selected = 0 then
      Status.Id_Selected := Status.Id_Top;
    end if;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Compute;

  -- Display the list, starting from FIRST_ITEM
  procedure Display (First_Item_Id : in Positive) is
    Item : Line_Rec;
  begin
    -- Set status
    Compute (First_Item_Id);

    if Line_List_Mng.Is_Empty (Line_List) then
      Set_Colors;
      Con_Io.Clear (List_Window);
      return;
    end if;

    -- Display list
    Move (Status.Id_Top);
    for I in 1 .. Status.Nb_Rows loop
      Get_Current_Item (Item);
      if not Af_Dscr.Fields(Lfn).Isprotected
      and then Status.Id_Top + I - 1 = Status.Id_Selected then
        Put (I - 1, Af_Ptg.Selected, Item);
      else
        Put (I - 1, Af_Ptg.Normal, Item);
      end if;
    end loop;
    Move (Status.Id_Selected);

    -- Display empty end of list (if any)
    for I in Status.Nb_Rows + 1 .. Af_Dscr.Fields(Lfn).Height loop
      Clear (I - 1);
    end loop;


  exception
    when others =>
      raise Afpx_Internal_Error;
  end Display;

  -- Actions on the list
  -- type ACTION_LIST is (UP, DOWN, PAGE_UP, PAGE_DOWN);

  -- Update the list due to an action
  procedure Update (Action : in List_Action_List) is
    First_Item_Id : Natural;
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    -- Update may be called before 1st PTG
    if Status.Id_Selected = 0 then
      Compute (1);
    end if;
    -- List is empty
    if Line_List_Mng.Is_Empty (Line_List) then
      return;
    end if;

    -- Update selection, cause current may have changed
    -- called by user
    Af_List.Set_Selected (Line_List_Mng.Get_Position(Line_List));

    -- Recompute cause list may have changed
    Compute (Status.Id_Top);

    -- Nothing to scroll
    if Status.Nb_Rows /= Af_Dscr.Fields(Lfn).Height then
      return;
    end if;

    case Action is
      when Up =>
        -- Scroll 1 row down
        if Status.Id_Top /= 1 then
          First_Item_Id := Status.Id_Top - 1;
          Display (First_Item_Id);
        end if;
      when Down =>
        -- Scroll 1 row down
        if Status.Id_Bottom /= Line_List_Mng.List_Length (Line_List) then
          First_Item_Id := Status.Id_Top + 1;
          Display (First_Item_Id);
        end if;
      when Page_Down =>
        -- Display next page
        -- Bottom + height < length => Bottom + height exists
        if Line_List_Mng.List_Length (Line_List) - Status.Id_Bottom >
        Af_Dscr.Fields(Lfn).Height then
          First_Item_Id := Status.Id_Top + Af_Dscr.Fields(Lfn).Height;
        elsif Status.Id_Bottom /= Line_List_Mng.List_Length (Line_List) then
          -- End at last item
          First_Item_Id := Line_List_Mng.List_Length (Line_List)
                           - Af_Dscr.Fields(Lfn).Height + 1;
        else
          -- Already at bottom of list
          return;
        end if;
        Display (First_Item_Id);
      when Page_Up =>
        -- Display previous page
        -- top - height > 1 => top - height exists
        if Status.Id_Top > Af_Dscr.Fields(Lfn).Height + 1 then
          First_Item_Id := Status.Id_Top - Af_Dscr.Fields(Lfn).Height;
        elsif Status.Id_Top /= 1 then
          -- Start at first item
          First_Item_Id := 1;
        else
          -- Already at top of list
          return;
        end if;
        Display (First_Item_Id);
      when Top =>
        -- Move to top of list
        if Status.Id_Top = 1 then
          -- Already at top of list
          return;
        end if;
        First_Item_Id := 1;
        Display (First_Item_Id);
      when Bottom =>
        -- Move to bottom of list
        if Status.Id_Bottom = Line_List_Mng.List_Length (Line_List) then
          -- Already at bottom of list
          return;
        end if;
        First_Item_Id := Line_List_Mng.List_Length (Line_List)
                         - Af_Dscr.Fields(Lfn).Height + 1;
        Display (First_Item_Id);
      when Center =>
        -- Center current LIST item in window (do ower best)
        declare
          -- List length
          Len : constant Positive := Line_List_Mng.List_Length (Line_List);
          -- Current position in list
          Pos : constant Positive
              := Line_List_Mng.Get_Position (Line_List);
          -- Row in window to put it
          Height : constant Positive := Af_Dscr.Fields(Lfn).Height;
          Midrow : constant Natural := Height / 2;
          Lastrow : constant Natural := Height - 1;
        begin
          if Pos - 1 < Midrow then
            Update(Top);
          elsif Len - Pos < Lastrow - Midrow then
            Update(Bottom);
          else
            First_Item_Id := Pos - Midrow;
            Display (First_Item_Id);
          end if;
        end;

    end case;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Update;

  -- Set the current item (selected_color) of the lis
  procedure Set_Selected (Item_Id : in Positive) is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Item_Id > Line_List_Mng.List_Length (Line_List) then
      raise Line_List_Mng.Not_In_List;
    end if;
    Status.Id_Selected := Item_Id;
  end Set_Selected;

  -- Status of the list
  function Get_Status return Status_Rec is
  begin
    return Status;
  end Get_Status;

  procedure Set_Current is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    if Line_List_Mng.Is_Empty (Line_List) then
      return;
    end if;
    Move (Status.Id_Selected);
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Set_Current;

  -- Is an ID, a row displayed
  function Id_Displayed (Id : Positive) return Boolean is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    return Id >= Status.Id_Top and then Id <= Status.Id_Bottom;
  end Id_Displayed;

  function Row_Displayed (Row : Con_Io.Row_Range) return Boolean is
  begin
    if not Opened then
      raise Not_Opened;
    end if;
    return Row < Status.Nb_Rows;
  end Row_Displayed;

  -- ROW <-> Item ID
  function To_Row (Id : Positive) return Con_Io.Row_Range is
  begin
    if not Id_Displayed (Id) then
      raise Afpx_Internal_Error;
    end if;
    return Id - Status.Id_Top;
  end To_Row;

  function To_Id  (Row : Con_Io.Row_Range) return Positive is
  begin
    if not Row_Displayed (Row) then
      raise Afpx_Internal_Error;
    end if;
    return Row + Status.Id_Top;
  end To_Id;

end Af_List;
