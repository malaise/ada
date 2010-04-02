with Ada.Calendar;
separate (Afpx)
package body Af_Ptg is

  type Mouse_Action_Rec(Kind : Afpx_Typ.Field_Kind_List := Afpx_Typ.Put)
  is record
    case Kind is
      when Afpx_Typ.Put =>
        -- Nothing to do, discarded, or list action already handled,
        -- no action put
        null;
      when Afpx_Typ.Button =>
        -- Double click in list
        -- Click and release in a Button field
        But_Field_No : Afpx_Typ.Absolute_Field_Range;
      when Afpx_Typ.Get =>
        -- Click and release in a Get field
        Get_Field_No : Afpx_Typ.Field_Range;
        Click_Col : Con_Io.Full_Col_Range;
    end case;
  end record;

  -- Button and Position of last click
  Last_But : List_Button_List;
  Last_Pos : Af_Con_Io.Full_Square;

  -- Time and list Id of first click
  Last_Selected_Id : Natural;
  Last_Selection_Time : Ada.Calendar.Time;
  Double_Click_Delay  : constant Ada.Calendar.Day_Duration := 0.2;

  -- Field and Col of selection request
  Selection_Field : Afpx_Typ.Field_Range;
  Selection_Insert : Boolean;
  Selection_Col : Af_Con_Io.Full_Col_Range;

  -- Sets Foreground and background according to state
  procedure Set_Colors (Field : in Afpx_Typ.Field_Rec;
                        State : in State_List;
                        Foreground : out Con_Io.Effective_Colors;
                        Background : out Con_Io.Effective_Colors) is
  begin
    -- Set colors
    case State is
      when Normal =>
        Foreground := Field.Colors.Foreground;
        Background := Field.Colors.Background;
      when Clicked =>
        Foreground := Field.Colors.Background;
        Background := Field.Colors.Foreground;
      when Selected =>
        Foreground := Field.Colors.Foreground;
        Background := Field.Colors.Selected;
    end case;
  end Set_Colors;

  -- Put a whole field in attribute
  procedure Put_Field (Field_No : in Afpx_Typ.Field_Range;
                       State    : in State_List) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Char_Index : Afpx_Typ.Char_Str_Range;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Colors;
  begin
    -- Set colors
    Set_Colors (Field, State, Foreground, Background);

    -- Set index to start of field's data
    Char_Index := Field.Char_Index;
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Af_Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      Af_Con_Io.Putw (
       S          => Af_Dscr.Chars(Char_Index .. Char_Index + Field.Width - 1),
       Name       => Af_Con_Io.Screen,
       Foreground => Af_Con_Io.Colors(Foreground),
       Blink_Stat => Af_Con_Io.Blink_Stats(Field.Colors.Blink_Stat),
       Background => Af_Con_Io.Colors(Background),
       Move       => False);
      -- Update Char_Index to first char of next row (except after last row)
      if I /= Field.Height then
        Char_Index := Char_Index + Field.Width;
      end if;
    end loop;
  end Put_Field;

  -- Erase a field (screen_background, screen_background)
  procedure Erase_Field (Field_No : in Afpx_Typ.Absolute_Field_Range) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Spaces : constant String (1 .. Field.Width) := (others => ' ');
  begin
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Af_Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      Af_Con_Io.Put (S          => Spaces,
                     Name       => Af_Con_Io.Screen,
                     Foreground => Af_Con_Io.Get_Background(Af_Con_Io.Screen),
                     Blink_Stat => Af_Con_Io.Not_Blink,
                     Background => Af_Con_Io.Get_Background(Af_Con_Io.Screen),
                     Move       => False);
    end loop;
  end Erase_Field;

  -- Col after last non space character of a field (or last col)
  function Last_Col (Field_No :  Afpx_Typ.Field_Range)
           return Af_Con_Io.Full_Col_Range is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Str : constant Wide_String (1 .. Field.Width)
        := Af_Dscr.Chars
              (Field.Char_Index .. Field.Char_Index + Field.Width - 1);
  begin
    return Last_Index (Str, True);
  end Last_Col;

  -- Click is valid if Left or Right button press
  -- Wheele and Middle button press are handled internally here
  function Valid_Click (List_Present : Boolean;
                        Cursor_Field : Afpx_Typ.Absolute_Field_Range;
                        Insert       : Boolean;
                        Right_Select : Boolean;
                        List_Change_Cb : access
        procedure (Action : in List_Change_List;
                   Status : in List_Status_Rec) := null) return Boolean is

    Mouse_Status : Af_Con_Io.Mouse_Event_Rec;
    Click_Pos : Af_Con_Io.Full_Square;
    Signif_Col : Af_Con_Io.Full_Col_Range;
    Valid : Boolean;
    List_Change : Boolean;
    use Af_Con_Io;
    use type Afpx_Typ.Field_Kind_List;
  begin
    -- Check if mouse button is clicked
    Af_Con_Io.Get_Mouse_Event (Mouse_Status);
    Click_Pos := (Mouse_Status.Row, Mouse_Status.Col);
    Valid := (        Mouse_Status.Button = Af_Con_Io.Left
              or else (Right_Select
                       and then Mouse_Status.Button = Af_Con_Io.Right) )
             and then Mouse_Status.Status = Af_Con_Io.Pressed;
    if Valid then
      if Mouse_Status.Button = Af_Con_Io.Left then
        Last_But := List_Left;
      else
        Last_But := List_Right;
      end if;
      Last_Pos := Click_Pos;
    else
      -- Handle wheele here: Click 4/5 in list
      if List_Present
      and then In_Field_Absolute(Lfn, Click_Pos)
      and then Mouse_Status.Status = Af_Con_Io.Pressed then
        case Mouse_Status.Button is
          when Af_Con_Io.Up =>
            List_Change := Af_List.Update (Up, True);
          when Af_Con_Io.Down =>
            List_Change := Af_List.Update (Down, True);
          when Af_Con_Io.Shift_Up =>
            List_Change := Af_List.Update (Page_Up, True);
          when Af_Con_Io.Shift_Down =>
            List_Change := Af_List.Update (Page_Down, True);
          when Af_Con_Io.Ctrl_Up =>
            List_Change := Af_List.Update (Shift_Page_Up, True);
          when Af_Con_Io.Ctrl_Down =>
            List_Change := Af_List.Update (Shift_Page_Down, True);
          when others =>
            List_Change := False;
        end case;
        if List_Change and then List_Change_Cb /= null then
          List_Change_Cb (Scroll, Af_List.Get_Status);
        end if;
      elsif Mouse_Status.Button = Middle
      and then Mouse_Status.Status = Af_Con_Io.Pressed
      and then Af_Dscr.Fields(Cursor_Field).Kind = Afpx_Typ.Get
      and then Af_Dscr.Fields(Cursor_Field).Activated
      and then not Af_Dscr.Fields(Cursor_Field).Isprotected
      and then In_Field_Absolute(Cursor_Field, Click_Pos) then
        -- Handle paste here: Click in middle of current get field
        Selection_Field := Cursor_Field;
        Selection_Insert := Insert;
        -- Insert where clicked if there is a significant char
        --  otherwise insert after last significant char
        Selection_Col := Click_Pos.Col
                       - Af_Dscr.Fields(Cursor_Field).Upper_Left.Col;
        Signif_Col := Last_Col (Selection_Field);
        if Selection_Col > Signif_Col then
          Selection_Col := Signif_Col;
        end if;
        Af_Con_Io.Request_Selection;
      end if;
    end if;
    return Valid;
  end Valid_Click;

  function Wait_Release (Button : List_Button_List)
                        return Af_Con_Io.Full_Square is
    Str : Wide_String (1 .. 0);
    Last : Natural;
    Stat : Af_Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    Mouse_Status : Af_Con_Io.Mouse_Event_Rec;
    use Af_Con_Io;
  begin
    -- Wait until button released
    loop
      Af_Con_Io.Get (Str, Last, Stat, Pos, Ins, Echo => False);
      if Stat = Af_Con_Io.Mouse_Button then
        Af_Con_Io.Get_Mouse_Event (Mouse_Status);
        exit when ( (Button = List_Left
              and then Mouse_Status.Button = Af_Con_Io.Left)
            or else (Button = List_Right
              and then Mouse_Status.Button = Af_Con_Io.Right) )
          and then Mouse_Status.Status = Af_Con_Io.Released;
      end if;
    end loop;
    -- Get pos when last released
    return (Mouse_Status.Row, Mouse_Status.Col);
  end Wait_Release;

  -- Handle a click action.
  -- Discard any position not in clickable field
  -- Reverse colors of field (or row of list)
  -- Wait for release
  -- Restore Button color
  -- Get selection is left to be achieved (restoring previous and setting
  --  new color)
  procedure Handle_Click (List_Present : in Boolean;
                          Cursor_Field : in Afpx_Typ.Absolute_Field_Range;
                          Insert       : in Boolean;
                          Right_Select : in Boolean;
                          Result : out Mouse_Action_Rec;
                          List_Change_Cb : access
        procedure (Action : in List_Change_List;
                   Status : in List_Status_Rec) := null) is
    Cursor_Pos : constant Af_Con_Io.Square := Af_Con_Io.Position;
    Valid_Field : Boolean;
    Click_Pos : Af_Con_Io.Full_Square;
    Click_But : List_Button_List;
    Click_Field : Afpx_Typ.Absolute_Field_Range;
    Release_Pos : Af_Con_Io.Full_Square;
    Click_Row_List : Af_Con_Io.Full_Row_Range;
    Click_On_Selected : Boolean;
    Field : Afpx_Typ.Field_Rec;
    List_Status : List_Status_Rec;
    Click_Time : Ada.Calendar.Time;
    Loc_Last_Selected_Id : Natural;

    List_Pos : Positive;
    procedure Save_Pos is
    begin
      List_Pos := Line_List.Get_Position;
    end Save_Pos;
    procedure Restore_Pos is
    begin
      Line_List.Move_At (List_Pos);
    end Restore_Pos;

    use Afpx_Typ;
    use Ada.Calendar;

  begin
    -- Save and reset last selected id
    Loc_Last_Selected_Id := Last_Selected_Id;
    Last_Selected_Id := 0;

    -- Result event discarded
    Result := (Kind => Afpx_Typ.Put);
    if not Valid_Click (List_Present, Cursor_Field, Insert, Right_Select,
                        List_Change_Cb) then
      return;
    end if;

    Valid_Field := True;
    -- Get button and pos, find field
    Click_But := Last_But;
    Click_Pos := Last_Pos;
    Click_Time := Ada.Calendar.Clock;
    if List_Present and then In_Field_Absolute(Lfn, Click_Pos)
    and then not Af_Dscr.Fields(Lfn).Isprotected then
      -- Selection in list
      Click_Field := Lfn;
      Click_Row_List := Click_Pos.Row - Af_Dscr.Fields(Lfn).Upper_Left.Row;
      List_Status := Af_List.Get_Status;
      -- Check that an item is displayed at this row
      if Click_Row_List >= List_Status.Nb_Rows then
        -- No data in this row
        Valid_Field := False;
      end if;
      -- Click on already selected item?
      if List_Status.Ids_Selected(Click_But) /= 0
      and then Af_List.Id_Displayed (List_Status.Ids_Selected(Click_But)) then
        Click_On_Selected :=
          Click_Row_List = Af_List.To_Row (List_Status.Ids_Selected(Click_But));
      else
        Click_On_Selected := False;
      end if;
    elsif Click_But = List_Left then
      -- Try to find a button or get field
      Click_Field := 0;
      for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
        if Af_Dscr.Fields(I).Kind /= Afpx_Typ.Put and then
           Af_Dscr.Fields(I).Activated            and then
           not Af_Dscr.Fields(I).Isprotected      and then
           In_Field_Absolute(I, Click_Pos) then
          Click_Field := I;
          exit;
        end if;
      end loop;
      if Click_Field = 0 then
        -- Invalid field
        Valid_Field := False;
      end if;
    else
      -- Right click not in list
      Valid_Field := False;
    end if;
    if Valid_Field then
      -- Reverse colors of field/row
      if Click_Field = Lfn then
        -- Reverse this row
        Save_Pos;
        Af_List.Put (Click_Row_List, Clicked, False);
        Restore_Pos;
      else
        -- Reverse this field
        Put_Field (Click_Field, Clicked);
      end if;
    end if;

    -- Wait release. No keyboard input
    Release_Pos := Wait_Release (Click_But);

    -- Done if click not valid
    if not Valid_Field then
      Af_Con_Io.Move (Cursor_Pos);
      return;
    end if;
    Field := Af_Dscr.Fields(Click_Field);

    -- Check release in same field/row as click
    if not In_Field_Absolute (Click_Field, Release_Pos) then
      Valid_Field := False;
    elsif Click_Field = Lfn and then Release_Pos.Row /= Click_Pos.Row then
      Valid_Field := False;
    end if;

    if Click_Field = Lfn then
      -- Click (& release) in list
      if Click_On_Selected then
        if not Valid_Field then
          -- Invalid release, restore initial selected
          Save_Pos;
          if Click_But = List_Left then
            Af_List.Put (Click_Row_List, Selected, False);
          else
            if List_Status.Ids_Selected(List_Right) /= 0 then
              Af_List.Put (Click_Row_List, Clicked, False);
            else
              Af_List.Put (Click_Row_List, Normal, False);
            end if;
          end if;
          Restore_Pos;
        elsif Click_But = List_Left
        and then Af_List.To_Id(Click_Row_List) = Loc_Last_Selected_Id
        and then Last_Selection_Time >= Click_Time - Double_Click_Delay then
          -- Double Left click
          Af_List.Put (Click_Row_List, Selected, False);
          Result := (Kind => Afpx_Typ.Button, But_Field_No => Click_Field);
        elsif Click_But = List_Left then
          -- Valid Left click. Store for next click to check double click
          Af_List.Put (Click_Row_List, Selected, False);
          List_Status := Af_List.Get_Status;
          Last_Selected_Id := List_Status.Ids_Selected(List_Left);
          Last_Selection_Time := Click_Time;
        else
          -- Valid right click on selected: flip/flop => Unselect
          Af_List.Set_Selected (Click_But, 0);
          Save_Pos;
          Af_List.Put (Click_Row_List, Normal, False);
          Restore_Pos;
          if List_Change_Cb /= null then
            List_Change_Cb (Right_Selection, Af_List.Get_Status);
          end if;
        end if;
      else
        -- Click on new row
        if not Valid_Field then
          -- Invalid release, restore clicked field as normal
          Save_Pos;
          Af_List.Put (Click_Row_List, Normal, False);
          Restore_Pos;
        else
          -- Valid release
          -- Un-select previous if it was shown
          if List_Status.Ids_Selected(Click_But) /= 0
          and then Af_List.Id_Displayed (List_Status.Ids_Selected(Click_But))
          then
            Save_Pos;
            Af_List.Put (Af_List.To_Row(List_Status.Ids_Selected(Click_But)),
                         Normal, False);
            Restore_Pos;
          end if;
          if Click_But = List_Left then
            -- Reset Right selected if Left clicking on it
            if Af_List.To_Id(Click_Row_List)
             = List_Status.Ids_Selected(List_Right) then
              Af_List.Set_Selected (List_Right, 0);
            end if;
            -- Set Left selected
            Af_List.Put (Click_Row_List, Selected, False);
            Af_List.Set_Selected (Click_But, Af_List.To_Id(Click_Row_List));
          elsif Af_List.To_Id(Click_Row_List)
                /= List_Status.Ids_Selected(List_Left) then
            -- Set right selected
            Af_List.Put (Click_Row_List, Clicked, False);
            Af_List.Set_Selected (Click_But, Af_List.To_Id(Click_Row_List));
          else
            -- Right click on left selection: no right selection
            Af_List.Put (Click_Row_List, Selected, False);
            Af_List.Set_Selected (Click_But, 0);
          end if;
          Af_List.Set_Current;
          if List_Change_Cb /= null then
            if Click_But = List_Left then
              List_Change_Cb (Left_Selection, Af_List.Get_Status);
            else
              List_Change_Cb (Right_Selection, Af_List.Get_Status);
            end if;
          end if;
        end if;
        if Click_But = List_Left then
          -- Valid Left click. Store for next click to check double click
          List_Status := Af_List.Get_Status;
          Last_Selected_Id := List_Status.Ids_Selected (List_Left);
          Last_Selection_Time := Click_Time;
        end if;
      end if;
      -- Result is Put
    elsif Field.Kind = Afpx_Typ.Get then
      -- If field is get: restore color if not valid
      if not Valid_Field then
        Put_Field (Click_Field, Normal);
        Result := (Kind => Afpx_Typ.Put);
      else
        Result := (Kind => Afpx_Typ.Get,
                   Get_Field_No => Click_Field,
                   Click_Col => Click_Pos.Col
                    - Af_Dscr.Fields(Click_Field).Upper_Left.Col);
      end if;
    else
      -- If field is button: restore color
      Put_Field (Click_Field, Normal);
      if Valid_Field then
        Result := (Kind => Afpx_Typ.Button, But_Field_No => Click_Field);
      end if;
    end if;

    -- Skip any keyboard entry during handle click
    Af_Con_Io.Move (Cursor_Pos);
  end Handle_Click;

  -- Get and past selection (if any) in Selection_Field
  --  and at Selection_Col
  -- Return -2 if selection failed (no change)
  -- Return -1 if end of field reached
  -- Otherwise return the new cursor col
  Sel_No_Change : constant Integer := -2;
  Sel_New_Field : constant Integer := -1;
  function Handle_Selection return Integer is
    Selection : constant String
              := Af_Con_Io.Get_Selection (Af_Con_Io.Full_Col_Range'Last + 1);
    -- Field and its initial content
    Field : constant Afpx_Typ.Field_Rec
          := Af_Dscr.Fields(Selection_Field);
    Str : Wide_String (1 .. Field.Width)
        := Af_Dscr.Chars
              (Field.Char_Index .. Field.Char_Index + Field.Width - 1);
    Available : Natural;
    Result : Integer;
  begin
    if Selection = "" then
      -- No selection
      return Sel_No_Change;
    end if;
    -- Amount of available columns remaining in field (for pasting selection)
    Available := Field.Width - Selection_Col;
    declare
      Sel : constant Wide_String := Language.String_To_Wide (Selection);
    begin
      if Sel'Length >= Available then
        -- Cursor will leave field (including when Sel'Length >= Available):
        --  insert Available characters
        Str(Selection_Col + 1 .. Selection_Col + Available)
           := Sel(Sel'First .. Sel'First + Available - 1);
        Result := Sel_New_Field;
      else
        -- Cursor will remain in field, insert Sel
        if Selection_Insert then
          -- Insert Sel: Overwrite up to Width with Sel and a Tail
          declare
            Tail : constant Wide_String
                 := Str(Selection_Col + 1 ..
                        Selection_Col + Available - Sel'Length);
          begin
            Str(Selection_Col + 1 .. Field.Width) := Sel & Tail;
          end;
        else
          -- Overwrite Sel'Lenght characters of Str
          Str(Selection_Col + 1 .. Selection_Col + Sel'Length) := Sel;
          Result := Selection_Col + Sel'Length;
        end if;
        Result := Selection_Col + Sel'Length;
        -- Remain in field
        if Result > Field.Width then
          Result := Field.Width - 1;
        end if;
      end if;
    end;

    -- 'Encode' new content
    Af_Dscr.Chars(Field.Char_Index .. Field.Char_Index + Field.Width - 1)
                 := Str;
    Af_Dscr.Current_Dscr.Modified := True;
    return Result;
  exception
    when others =>
      return Sel_No_Change;
  end Handle_Selection;

  -- Call the user callback to get cursor col
  function Get_Cursor_Col (
                 Field_No : Afpx_Typ.Field_Range;
                 New_Field : Boolean;
                 Cursor_Col : Con_Io.Full_Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Cursor_Col_Cb : access
    function (Cursor_Field : Field_Range;
              New_Field : Boolean;
              Cursor_Col : Con_Io.Full_Col_Range;
              Enter_Field_Cause : Enter_Field_Cause_List;
              Str : Wide_String) return Con_Io.Full_Col_Range)
  return Af_Con_Io.Full_Col_Range is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields (Field_No);
    Result : Af_Con_Io.Col_Range;
    Signif_Col : Af_Con_Io.Full_Col_Range;
  begin
    -- Call Cb if set
    if Cursor_Col_Cb = null then
      case Enter_Field_Cause is
        when Right_Full | Tab | Stab=>
          -- Start of field if Right_Full, Tab or Stab
          return Con_Io.Full_Col_Range'First;
        when Left =>
          -- Last significant col if Left
          Signif_Col := Last_Col (Field_No);
          return Signif_Col;
        when Mouse =>
          -- When click in new field set cursor where clicked if there is a
          --  significant char otherwise set it just after last significant char
          -- When same field, set cursor where clicked
          Signif_Col := Last_Col (Field_No);
          if Cursor_Col > Signif_Col then
            return Signif_Col;
          else
            return Cursor_Col;
          end if;
      end case;
    end if;
    -- The user Cb will appreciate a string (1 .. Len)
    --  so make a local copy
    declare
      Str : constant Wide_String (1 .. Field.Width)
          := Af_Dscr.Chars
              (Field.Char_Index .. Field.Char_Index + Field.Width - 1);
    begin
      Result := Cursor_Col_Cb (Afpx.Field_Range(Field_No),
                               New_Field,
                               Cursor_Col,
                               Enter_Field_Cause,
                               Str);
    end;
    -- Check result vs width
    if Result >= Field.Width then
      Result := Field.Width - 1;
    end if;
    return Result;
  exception
    when others =>
      return Field.Width - 1;
  end Get_Cursor_Col;

  -- Print the fields and the list, then gets
  procedure Ptg (Cursor_Field  : in out Afpx_Typ.Field_Range;
                 Cursor_Col    : in out Af_Con_Io.Col_Range;
                 Insert        : in out Boolean;
                 Result        : out Result_Rec;
                 Redisplay     : in Boolean;
                 Right_Select  : in Boolean;
                 Get_Active    : in Boolean;

                 Cursor_Col_Cb : access
     function (Cursor_Field : Field_Range;
               New_Field : Boolean;
               Cursor_Col : Con_Io.Full_Col_Range;
               Enter_Field_Cause : Enter_Field_Cause_List;
               Str : Wide_String) return Con_Io.Full_Col_Range := null;

                 List_Change_Cb : access
     procedure (Action : in List_Change_List;
                Status : in List_Status_Rec) := null) is

    List_Present : Boolean;
    New_Field : Boolean;
    Field : Afpx_Typ.Field_Rec;
    Last : Natural;
    Stat : Af_Con_Io.Curs_Mvt;
    Pos : Positive;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Colors;
    Done : Boolean;
    Need_Redisplay : Boolean;
    Selection_Result : Integer;
    List_Change : Boolean;

    use Afpx_Typ;
    use type Af_Con_Io.Curs_Mvt;

  begin
    -- Reset last selection for double click
    Last_Selected_Id := 0;

    -- Erase potential garbage if redisplay
    if Redisplay then
      Af_Con_Io.Clear (Af_Con_Io.Screen);
    end if;

    -- A new field at start up if some get field
    New_Field := Get_Active;

    Need_Redisplay := Redisplay;

    -- The infinite loop
    loop

      -- List present : defined, activated and not empty
      List_Present := Af_Dscr.Has_List
             and then Af_Dscr.Fields(Lfn).Activated
             and then not Line_List.Is_Empty;

      -- Update list if Line_List has changed list if needed
      if List_Present then
        if Af_List.Get_Status.Ids_Selected(List_Left)
              /= Line_List.Get_Position then
          Af_List.Set_Selected (List_Left, Line_List.Get_Position);
          if Af_List.Get_Status.Ids_Selected(List_Left)
           = Af_List.Get_Status.Ids_Selected(List_Right) then
            -- User has moved selection to Id_Selected_Right,
            --  -> Reset Id_Selected_Right
            Af_List.Set_Selected (List_Right, 0);
          end if;
        end if;
      elsif Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
        -- List exists but is empty or inactive
        Af_List.Set_Selected (List_Left, 0);
        Af_List.Set_Selected (List_Right, 0);
      end if;

      -- list to be updated if Line_List has changed
      if Line_List.Is_Modified then
        Af_List.Modified := True;
        Line_List.Modification_Ack;
      end if;

      if (Need_Redisplay or else Af_List.Modified)
      and then Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
        List_Change := True;
        if not Af_Dscr.Has_List then
          -- No list in this Dscr
          List_Change := False;
        elsif Line_List.Is_Empty then
          -- Empty list
          Af_List.Display(1);
        elsif not Af_Dscr.Fields(Lfn).Activated then
          -- List not active
          Erase_Field (Lfn);
          Af_List.Modified := False;
        else
          -- List is present (exists, active and not empty)
          if Af_List.Get_Status.Id_Top = 0 then
            -- First time we display a non empty list
            Af_List.Display(1);
          else
            -- Redisplay
            Af_List.Display(Af_List.Get_Status.Id_Top);
          end if;
        end if;
        if List_Change and then List_Change_Cb /= null then
          List_Change_Cb (List_Modified, Af_List.Get_Status);
        end if;
      end if;

      -- Redisplay all fields if requested or needed
      if Need_Redisplay or else Af_Dscr.Current_Dscr.Modified then
        for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
          if Af_Dscr.Fields(I).Activated then
            Put_Field (I, Normal);
          else
            Erase_Field (I);
          end if;
        end loop;
      end if;
      Af_Dscr.Current_Dscr.Modified := False;

      -- No more forced redisplay
      Need_Redisplay := False;

      -- Get field, set colors when field changes
      if New_Field then
        Field := Af_Dscr.Fields(Cursor_Field);
        Set_Colors (Field, Selected, Foreground, Background);
        New_Field := False;
      end if;
      if Get_Active then
        Pos := Cursor_Col + 1;
        -- Move at beginning of field and put_then_get
        Af_Con_Io.Move (Field.Upper_Left.Row, Field.Upper_Left.Col);
        Af_Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars
                    (Field.Char_Index .. Field.Char_Index + Field.Width - 1),
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Insert,
         Name   => Af_Con_Io.Screen,
         Foreground => Af_Con_Io.Colors(Foreground),
         Blink_Stat => Af_Con_Io.Blink_Stats(Field.Colors.Blink_Stat),
         Background => Af_Con_Io.Colors(Background));
        Cursor_Col := Pos - 1;
      else
        -- Blind get
        Insert := False;
        Af_Con_Io.Move (Af_Con_Io.Row_Range'Last, Af_Con_Io.Col_Range'Last);
        Af_Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars (1 .. 0),
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Insert,
         Name   => Af_Con_Io.Screen,
         Foreground => Af_Con_Io.Get_Background(Af_Con_Io.Screen),
         Blink_Stat => Af_Con_Io.Not_Blink,
         Background => Af_Con_Io.Get_Background(Af_Con_Io.Screen));
       end if;

      Done := False;
      -- Now the big case on keys
      List_Change := False;
      case Stat is
        when Af_Con_Io.Up =>
          -- List scroll down
          if List_Present then
            List_Change := Af_List.Update (Up, True);
          end if;
        when Af_Con_Io.Down =>
          -- List scroll up
          if List_Present then
            List_Change := Af_List.Update (Down, True);
          end if;
        when Af_Con_Io.Pgup | Af_Con_Io.Ctrl_Up =>
          -- List page up
          if List_Present then
            List_Change := Af_List.Update (Page_Up, True);
          end if;
        when Af_Con_Io.Pgdown | Af_Con_Io.Ctrl_Down =>
          -- List page down
          if List_Present then
            List_Change := Af_List.Update (Page_Down, True);
          end if;
        when Af_Con_Io.Ctrl_Pgup =>
          -- List page up
          if List_Present then
            List_Change := Af_List.Update (Top, True);
          end if;
        when Af_Con_Io.Ctrl_Pgdown =>
          -- List page down
          if List_Present then
            List_Change := Af_List.Update (Bottom, True);
          end if;
        when Af_Con_Io.Right | Af_Con_Io.Full =>
          if Get_Active then
            -- End (right) of previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Next_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                          True,
                                          Af_Con_Io.Col_Range'First,
                                          Right_Full, Cursor_Col_Cb);
            New_Field := True;
            Insert := False;
          end if;
        when Af_Con_Io.Tab | Af_Con_Io.Ctrl_Right =>
          if Get_Active then
            -- Tab in previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Next_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                          True,
                                          Af_Con_Io.Col_Range'First,
                                          Tab, Cursor_Col_Cb);
            New_Field := True;
            Insert := False;
          end if;
        when Af_Con_Io.Left =>
          if Get_Active then
            -- Left on previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Prev_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                          True,
                                          Af_Con_Io.Col_Range'First,
                                          Left, Cursor_Col_Cb);
            New_Field := True;
            Insert := False;
          end if;
        when Af_Con_Io.Stab | Af_Con_Io.Ctrl_Left =>
          if Get_Active then
            -- Beginning of prev get field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Prev_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                          True,
                                          Af_Con_Io.Col_Range'First,
                                          Stab, Cursor_Col_Cb);
            New_Field := True;
            Insert := False;
          end if;
        when Af_Con_Io.Ret =>
          -- End put_then_get on keyboard ret
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Return_Key);
          Insert := False;
          Done := True;
        when Af_Con_Io.Esc =>
          -- End put_then_get on keyboard esc
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Escape_Key);
          Insert := False;
          Done := True;
        when Af_Con_Io.Selection =>
          -- Paste selection
          if Get_Active  then
            Selection_Result := Handle_Selection;
            if Selection_Result = Sel_New_Field then
              -- Selection completed the field
              -- Restore normal color of previous field
              Put_Field (Cursor_Field, Normal);
              Cursor_Field := Next_Get_Field (Cursor_Field);
              Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                            True,
                                            Af_Con_Io.Col_Range'First,
                                            Right_Full, Cursor_Col_Cb);
              New_Field := True;
              Insert := False;
            elsif Selection_Result /= Sel_No_Change then
              -- Selection moved the cursor
              Cursor_Col := Selection_Result;
            end if;
          end if;
        when Af_Con_Io.Mouse_Button =>
          declare
            Click_Result : Mouse_Action_Rec;
          begin
            Handle_Click (List_Present, Cursor_Field, Insert, Right_Select,
                          Click_Result, List_Change_Cb);
            case Click_Result.Kind is
              when Afpx_Typ.Put =>
                null;
              when Afpx_Typ.Get =>
                if Click_Result.Get_Field_No /= Cursor_Field then
                  -- Restore normal color of previous field
                  Put_Field (Cursor_Field, Normal);
                  -- Change field
                  Cursor_Field := Click_Result.Get_Field_No;
                  -- Update cursor col
                  Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                                True,
                                                Click_Result.Click_Col,
                                                Mouse, Cursor_Col_Cb);
                  New_Field := True;
                  Insert := False;
                else
                  -- Same field, update cursor col
                  Cursor_Col := Get_Cursor_Col (Cursor_Field,
                                                False,
                                                Click_Result.Click_Col,
                                                Mouse, Cursor_Col_Cb);
                end if;
              when Afpx_Typ.Button =>
                -- End of put_then_get
                if List_Present then
                  Af_List.Set_Current;
                end if;
                Result := (Id_Selected_Right  =>
                             Af_List.Get_Status.Ids_Selected (List_Right),
                           Event        => Mouse_Button,
                           Field_No =>
                              Absolute_Field_Range(Click_Result.But_Field_No));
                Insert := False;
                Done := True;
            end case;
          end;
        when Af_Con_Io.Break =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Break_Key);
          Insert := False;
          Done := True;
        when Af_Con_Io.Refresh =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Refresh);
          Done := True;
        when Af_Con_Io.Fd_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Fd_Event);
          Done := True;
        when Af_Con_Io.Timer_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Timer_Event);
          Done := True;
        when Af_Con_Io.Signal_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Signal_Event);
          Done := True;
        when Af_Con_Io.Timeout =>
          null;
      end case;

      -- Notify of change of list because of key
      if List_Change and then List_Change_Cb /= null then
        List_Change_Cb (Scroll, Af_List.Get_Status);
      end if;

      exit when Done;
    end loop;

  end Ptg;

end Af_Ptg;

