with Calendar;
separate (Afpx)
package body Af_Ptg is

  type Mouse_Action_Rec(Kind : Afpx_Typ.Field_Kind_List := Afpx_Typ.Put)
  is record
    case Kind is
      when Afpx_Typ.Put =>
        -- Nothing to do, discarded, or list action already handled,
        -- no action put
        null;
      when Afpx_Typ.Get | Afpx_Typ.Button =>
        -- Click and release in a Get field
        -- Double click in list
        -- Click and release in a Button field
        Field_No : Afpx_Typ.Absolute_Field_Range;
    end case;
  end record;

  Last_Pos : Con_Io.Square;

  Last_Selected_Id : Natural;
  Last_Selection_Time : Calendar.Time;
  Double_Click_Delay  : constant Calendar.Day_Duration := 0.2;

  -- Sets Foreground and background according to state
  procedure Set_Colors (Field : in Afpx_Typ.Field_Rec;
                        State : in State_List;
                        Foreground : out Con_Io.Effective_Colors;
                        Background : out Con_Io.Effective_Basic_Colors) is
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
    Background : Con_Io.Effective_Basic_Colors;
  begin
    -- Set colors
    Set_Colors (Field, State, Foreground, Background);

    -- Set index to start of field's data
    Char_Index := Field.Char_Index;
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      Con_Io.Put (
        S => Af_Dscr.Chars(Char_Index .. Char_Index + Field.Width - 1),
        Name       => Con_Io.Screen,
        Foreground => Foreground,
        Blink_Stat => Field.Colors.Blink_Stat,
        Background => Background,
        Move       => False);
      -- Update Char_Index to first char of next row (except after last row)
      if I /= Field.Height then
        Char_Index := Char_Index + Field.Width;
      end if;
    end loop;
  end Put_Field;

  -- Put a whole row of a field in attribute
  procedure Put_Row (Field_No : in Afpx_Typ.Field_Range;
                     Row      : in Con_Io.Row_Range;
                     State    : in State_List) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Char_Index : Afpx_Typ.Char_Str_Range;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Basic_Colors;
  begin
    -- Set colors
    Set_Colors (Field, State, Foreground, Background);
    -- Check Row in field
    if not Afpx_Typ.In_Field (Field, (Row, 0)) then
      raise Afpx_Internal_Error;
    end if;

    -- Set index to start of row's data
    Char_Index := Field.Char_Index + Row * Field.Width;
    -- Go to row, left of field
    Con_Io.Move (Field.Upper_Left.Row + Row, Field.Upper_Left.Col);
    Con_Io.Put (
      S => Af_Dscr.Chars(Char_Index .. Char_Index + Field.Width - 1),
      Name       => Con_Io.Screen,
      Foreground => Foreground,
      Blink_Stat => Field.Colors.Blink_Stat,
      Background => Background,
      Move       => False);
  end Put_Row;

  -- Put a string somwhere in a field
  procedure Put_Str (Field_No : in Afpx_Typ.Field_Range;
                     Pos      : in Con_Io.Square;
                     Str      : in String;
                     State    : in State_List) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Char_Index : Afpx_Typ.Char_Str_Range;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Basic_Colors;
    Len        : Positive;
  begin
    -- Set colors
    Set_Colors (Field, State, Foreground, Background);
    -- Check Pos is in field
    if not Afpx_Typ.In_Field (Field, (Pos.Row, Pos.Col)) then
      raise Afpx_Internal_Error;
    end if;

    -- Set index to start of row's data
    Char_Index := Field.Char_Index + Pos.Row * Field.Width + Pos.Col;
    -- Go to row, left of field
    Con_Io.Move (Field.Upper_Left.Row + Pos.Row,
                 Field.Upper_Left.Col + Pos.Col);
    -- Adjust str length to truncate it if is too long
    if Afpx_Typ.In_Field (Field, (Pos.Row, Pos.Col + Str'Length - 1)) then
      Len := Str'Length;
    else
      Len := Field.Width - Pos.Col;
    end if;

    Con_Io.Put (
      S          => Str (Str'First .. Str'First + Len - 1),
      Name       => Con_Io.Screen,
      Foreground => Foreground,
      Blink_Stat => Field.Colors.Blink_Stat,
      Background => Background,
      Move       => False);
  end Put_Str;

  -- Erase a field (screen_background, screen_background)
  procedure Erase_Field (Field_No : in Afpx_Typ.Absolute_Field_Range) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Spaces : constant String (1 .. Field.Width) := (others => ' ');
  begin
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      Con_Io.Put (S          => Spaces,
                  Name       => Con_Io.Screen,
                  Foreground => Con_Io.Get_Background(Con_Io.Screen),
                  Blink_Stat => Con_Io.Not_Blink,
                  Background => Con_Io.Get_Background(Con_Io.Screen),
                  Move       => False);
    end loop;
  end Erase_Field;

  function Valid_Click (List_Present : in Boolean) return Boolean is
    Mouse_Status : Con_Io.Mouse_Event_Rec;
    Click_Pos : Con_Io.Square;
    Valid : Boolean;
    use Con_Io;
  begin
    -- Check if mouse button is clicked
    Con_Io.Get_Mouse_Event (Mouse_Status);
    Click_Pos := (Mouse_Status.Row, Mouse_Status.Col);
    Valid := Mouse_Status.Button = Con_Io.Left
             and then Mouse_Status.Status = Con_Io.Pressed;
    if Valid then
      Last_Pos := Click_Pos;
    else
      -- Handle wheele here: Click 4/5 in list
      if List_Present
      and then In_Field_Absolute(Lfn, Click_Pos)
      and then Mouse_Status.Status = Con_Io.Pressed then
        if Mouse_Status.Button = Con_Io.Up then
          Af_List.Update (Up);
        elsif Mouse_Status.Button = Con_Io.Down then
          Af_List.Update (Down);
        end if;
      end if;
    end if;
    return Valid;
  end Valid_Click;

  function Last_Click return Con_Io.Square is
  begin
    return Last_Pos;
  end Last_Click;

  function Wait_Release return Con_Io.Square is
    Str : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    Mouse_Status : Con_Io.Mouse_Event_Rec;
    use Con_Io;
  begin
    -- Wait until button released
    loop
      Con_Io.Get (Str, Last, Stat, Pos, Ins, Echo => False);
      if Stat = Con_Io.Mouse_Button then
        Con_Io.Get_Mouse_Event (Mouse_Status);
        exit when Mouse_Status.Button = Con_Io.Left
             and then Mouse_Status.Status = Con_Io.Released;
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
                          Result : out Mouse_Action_Rec) is
    Cursor_Pos : constant Con_Io.Square := Con_Io.Position;
    Valid_Field : Boolean;
    Click_Pos : Con_Io.Square;
    Click_Field : Afpx_Typ.Absolute_Field_Range;
    Release_Pos : Con_Io.Square;
    Click_Row_List : Con_Io.Row_Range;
    Click_On_Selected : Boolean;
    Field : Afpx_Typ.Field_Rec;
    List_Status : Af_List.Status_Rec;
    Click_Time : Calendar.Time;
    Loc_Last_Selected_Id : Natural;
    use Afpx_Typ;
    use Calendar;

  begin
    -- Save and reset last selected id
    Loc_Last_Selected_Id := Last_Selected_Id;
    Last_Selected_Id := 0;

    -- Result event discarded
    Result := (Kind => Afpx_Typ.Put);
    if not Valid_Click (List_Present) then
      return;
    end if;

    Valid_Field := True;
    -- Get pos, find field
    Click_Pos := Last_Click;
    Click_Time := Calendar.Clock;
    if List_Present and then In_Field_Absolute(Lfn, Click_Pos)
    and then not Af_Dscr.Fields(Lfn).Isprotected then
      Click_Field := Lfn;
      Click_Row_List := Click_Pos.Row - Af_Dscr.Fields(Lfn).Upper_Left.Row;
      List_Status := Af_List.Get_Status;
      -- Check that an item is displayed at this row
      if Click_Row_List >= List_Status.Nb_Rows then
        -- No data in this row
        Valid_Field := False;
      end if;
      -- Click on already selected item?
      if Af_List.Id_Displayed (List_Status.Id_Selected) then
        Click_On_Selected :=
          Click_Row_List = Af_List.To_Row (List_Status.Id_Selected);
      else
        Click_On_Selected := False;
     end if;
    else
      -- Try to find a button
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
    end if;
    if Valid_Field then
      -- reverse colors of field/row
      if Click_Field = Lfn then
        -- Reverse
        Af_List.Put (Click_Row_List, Clicked);
      else
        Put_Field (Click_Field, Clicked);
      end if;
    end if;

    -- Wait release. No keyboard input
    Release_Pos := Wait_Release;

    -- Done if click not valid
    if not Valid_Field then
      Con_Io.Move (Cursor_Pos);
      return;
    end if;
    Field := Af_Dscr.Fields(Click_Field);

    -- Check release in same field/row than click
    if not In_Field_Absolute (Click_Field, Release_Pos) then
      Valid_Field := False;
    elsif Click_Field = Lfn and then Release_Pos.Row /= Click_Pos.Row then
      Valid_Field := False;
    end if;

    if Click_Field = Lfn then
      if Click_On_Selected then
        -- Valid or not, restore selected
        Af_List.Put (Click_Row_List, Selected);
        -- Check double_click
        if Af_List.To_Id(Click_Row_List) = Loc_Last_Selected_Id
          and then Last_Selection_Time >= Click_Time - Double_Click_Delay then
            Result := (Kind => Afpx_Typ.Button, Field_No => Click_Field);
        else
          -- Valid click. Store for next click to check double click
          List_Status := Af_List.Get_Status;
          Last_Selected_Id := List_Status.Id_Selected;
          Last_Selection_Time := Click_Time;
        end if;
      else
        if not Valid_Field then
          -- Invalid release, restore clicked field as normal
          Af_List.Put (Click_Row_List, Normal);
        else
          -- Valid release
          -- Un-select previous if it was shown
          if Af_List.Id_Displayed (List_Status.Id_Selected) then
            Af_List.Put (Af_List.To_Row(List_Status.Id_Selected), Normal);
          end if;
          -- change selected if valid and new
          Af_List.Put (Click_Row_List, Selected);
          -- Set new selected
          Af_List.Set_Selected (Af_List.To_Id(Click_Row_List));
          Af_List.Set_Current;
        end if;
        -- Valid click. Store for next click to check double click
        List_Status := Af_List.Get_Status;
        Last_Selected_Id := List_Status.Id_Selected;
        Last_Selection_Time := Click_Time;
      end if;
      -- Result is Put
    elsif Field.Kind = Afpx_Typ.Get then
      -- If field is get: restore color if not valid
      if not Valid_Field then
        Put_Field (Click_Field, Normal);
        Result := (Kind => Afpx_Typ.Put);
      else
        Result := (Kind => Afpx_Typ.Get, Field_No => Click_Field);
      end if;
    else
      -- If field is button: restore color
      Put_Field (Click_Field, Normal);
      if Valid_Field then
        Result := (Kind => Afpx_Typ.Button, Field_No => Click_Field);
      end if;
    end if;

    -- Skip any keyboard entry during handle click
    Con_Io.Move (Cursor_Pos);
  end Handle_Click;

  -- Call the user callback to get cursor col
  function Get_Cursor_Col (
                 Field : Afpx_Typ.Field_Rec;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Cursor_Col_Cb : Cursor_Set_Col_Cb) 
           return Con_Io.Col_Range is
    Result : Con_Io.Col_Range;
  begin
    -- Call Cb if set
    if Cursor_Col_Cb = null then
      -- End of field if Left, Start of field otherwise
      if Enter_Field_Cause = Left then
         return Field.Width - 1;
      else
        return 0;
      end if;
    end if;
    -- The user user Cb will appreciate a string (1 .. Len)
    --  so make a local copy
    declare
      Str : constant String (1 .. Field.Width) := Af_Dscr.Chars
            (Field.Char_Index .. Field.Char_Index + Field.Width - 1);
    begin
      Result := Cursor_Col_Cb (Enter_Field_Cause, Str);
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
  procedure Ptg (
                 Cursor_Field  : in out Afpx_Typ.Field_Range;
                 Cursor_Col    : in out Con_Io.Col_Range;
                 Result        : out Result_Rec;
                 Redisplay     : in Boolean;
                 Get_Active    : in Boolean;
                 Cursor_Col_Cb : in Cursor_Set_Col_Cb) is
    List_Present : Boolean;
    New_Field : Boolean;
    Field : Afpx_Typ.Field_Rec;
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Insert : Boolean;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Basic_Colors;
    Done : Boolean;

    use Afpx_Typ; 
    use type Con_Io.Curs_Mvt;

  begin
    -- Reset last selection for double click
    Last_Selected_Id := 0;

    -- Erase potential garbage if redisplay
    if Redisplay then
      Con_Io.Clear (Con_Io.Screen);
    end if;

    -- List present : defined, activated and not empty
    List_Present := Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button
           and then Af_Dscr.Fields(Lfn).Activated
           and then not Line_List_Mng.Is_Empty(Line_List);
    -- Init list if needed
    if List_Present then
      Af_List.Set_Selected (Line_List_Mng.Get_Position(Line_List));
    end if;

    -- Redisplay list if requested or needed
    if (Redisplay or else Line_List_Mng.Is_Modified (Line_List))
    and then Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
      -- list defined
      if List_Present then
        if Af_List.Get_Status.Id_Top = 0 then
          -- First time we display a non empty list
          Af_List.Display(1);
        else
          Af_List.Display(Af_List.Get_Status.Id_Top);
        end if;
      elsif not Af_Dscr.Fields(Lfn).Activated then
        -- List not active
        Erase_Field (Lfn);
      else
        -- Empty list
        Af_List.Display(1);
      end if;
    end if;

    -- Redisplay all fields if requested or needed
    if Redisplay or else Af_Dscr.Current_Dscr.Modified then
      for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
        if Af_Dscr.Fields(I).Activated then
          Put_Field (I, Normal);
        else
          Erase_Field (I);
        end if;
      end loop;
    end if;

    -- A new field at start up if some get field
    New_Field := Get_Active;

    -- The infinite loop
    loop
      -- Get field, set colors when field changes
      if New_Field then
        Field := Af_Dscr.Fields(Cursor_Field);
        Set_Colors (Field, Selected, Foreground, Background);
        Insert := False;
        New_Field := False;
      end if;
      if Get_Active then
        Pos := Cursor_Col + 1;
        -- Move at beginning of field and put_then_get
        Con_Io.Move (Field.Upper_Left.Row, Field.Upper_Left.Col);
        Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars
                    (Field.Char_Index .. Field.Char_Index + Field.Width - 1),
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Insert,
         Name   => Con_Io.Screen,
         Foreground => Foreground,
         Blink_Stat => Field.Colors.Blink_Stat,
         Background => Background);
        Cursor_Col := Pos - 1;
      else
        -- Blind get
        Con_Io.Move (Con_Io.Row_Range'Last, Con_Io.Col_Range'Last);
        Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars (1 .. 0),
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Insert,
         Name   => Con_Io.Screen,
         Foreground => Con_Io.Get_Background(Con_Io.Screen),
         Blink_Stat => Con_Io.Not_Blink,
         Background => Con_Io.Get_Background(Con_Io.Screen));
       end if;

      Done := False;
      -- Now the big case
      case Stat is
        when Con_Io.Up =>
          -- List scroll down
          if List_Present then
            Af_List.Update (Up);
          end if;
        when Con_Io.Down =>
          -- List scroll up
          if List_Present then
            Af_List.Update (Down);
          end if;
        when Con_Io.Pgup =>
          -- List page up
          if List_Present then
            Af_List.Update (Page_Up);
          end if;
        when Con_Io.Pgdown =>
          -- List page down
          if List_Present then
            Af_List.Update (Page_Down);
          end if;
        when Con_Io.Ctrl_Pgup =>
          -- List page up
          if List_Present then
            Af_List.Update (Top);
          end if;
        when Con_Io.Ctrl_Pgdown =>
          -- List page down
          if List_Present then
            Af_List.Update (Bottom);
          end if;
        when Con_Io.Right | Con_Io.Full =>
          if Get_Active then
            -- End (right) of previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Next_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Af_Dscr.Fields(Cursor_Field),
                                          Right_Full, Cursor_Col_Cb);
            New_Field := True;
          end if;
        when Con_Io.Tab =>
          if Get_Active then
            -- Tab in previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Next_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Af_Dscr.Fields(Cursor_Field),
                                          Tab, Cursor_Col_Cb);
            New_Field := True;
          end if;
        when Con_Io.Left =>
          if Get_Active then
            -- Left on previous field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Prev_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Af_Dscr.Fields(Cursor_Field),
                                          Left, Cursor_Col_Cb);
            New_Field := True;
          end if;
        when Con_Io.Stab =>
          if Get_Active then
            -- Beginning of prev get field
            -- Restore normal color of previous field
            Put_Field (Cursor_Field, Normal);
            Cursor_Field := Prev_Get_Field (Cursor_Field);
            Cursor_Col := Get_Cursor_Col (Af_Dscr.Fields(Cursor_Field),
                                          Stab, Cursor_Col_Cb);
            New_Field := True;
          end if;
        when Con_Io.Ret =>
          -- End put_then_get on keyboard ret
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Keyboard,
                     Keyboard_Key => Return_Key);
          Done := True;
        when Con_Io.Esc =>
          -- End put_then_get on keyboard esc
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Keyboard,
                     Keyboard_Key => Escape_Key);
          Done := True;
        when Con_Io.Mouse_Button =>
          declare
            Click_Result : Mouse_Action_Rec;
          begin
            Handle_Click (List_Present, Click_Result);
            case Click_Result.Kind is
              when Afpx_Typ.Put =>
                null;
              when Afpx_Typ.Get =>
                if Click_Result.Field_No /= Cursor_Field then
                  -- Restore normal color of previous field
                  Put_Field (Cursor_Field, Normal);
                  -- Change field
                  Cursor_Field := Click_Result.Field_No;
                  Cursor_Col := Get_Cursor_Col (Af_Dscr.Fields(Cursor_Field),
                                                Mouse, Cursor_Col_Cb);
                  New_Field := True;
                end if;
              when Afpx_Typ.Button =>
                -- End of put_then_get
                if List_Present then
                  Af_List.Set_Current;
                end if;
                Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                           Event        => Mouse_Button,
                           Field_No     => Absolute_Field_Range(Click_Result.Field_No));
                Done := True;
            end case;
          end;
        when Con_Io.Break =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Keyboard,
                     Keyboard_Key => Break_Key);
          Done := True;
        when Con_Io.Refresh =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Refresh);
          Done := True;
        when Con_Io.Fd_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Fd_Event);
          Done := True;
        when Con_Io.Timer_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Timer_Event);
          Done := True;
        when Con_Io.Signal_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected  => Af_List.Get_Status.Id_Selected,
                     Event        => Signal_Event);
          Done := True;
        when Con_Io.Timeout =>
          null;
      end case;

      exit when Done;
    end loop;

    Af_Dscr.Current_Dscr.Modified := False;
  end Ptg;

end Af_Ptg;

