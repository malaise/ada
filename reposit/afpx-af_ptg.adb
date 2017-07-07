with Ada.Calendar;
with Aski;
separate (Afpx)
package body Af_Ptg is

  type Mouse_Action_Rec(Kind : Field_Kind_List := Put_Field)
  is record
    case Kind is
      when Put_Field =>
        -- Nothing to do, discarded, or list action already handled,
        -- no action put
        null;
      when Button_Field =>
        -- Double click in list
        -- Click and release in a Button field
        But_Field_No : Absolute_Field_Range;
        But_Click_Square, But_Release_Square : Con_Io.Square;
      when Get_Field =>
        -- Click and release in a Get field
        Get_Field_No : Field_Range;
        Click_Col : Con_Io.Col_Range;
    end case;
  end record;

  -- Button and Position of last click
  subtype Button_List is Con_Io.Mouse_Pointing_List;
  Last_But : Button_List;
  Last_Pos : Con_Io.Square;

  -- Time and list Id of first click
  Last_Selected_Id : Natural;
  Last_Selection_Time : Ada.Calendar.Time;
  Double_Click_Delay : Double_Click_Delay_Range
                     := Default_Double_Click_Delay;
  Show_Click_Delay  : constant Ada.Calendar.Day_Duration := 0.03;

  -- Cursor field at end of prev Ptg
  Prev_Cursor_Field : Absolute_Field_Range := List_Field_No;

  -- Need to redisplay at next Ptg
  Need_Redisplay : Boolean := False;

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

  -- Set double-click delay
  procedure Set_Double_Click_Delay (
      Double_Click_Delay : in Double_Click_Delay_Range) is
  begin
    Af_Ptg.Double_Click_Delay := Double_Click_Delay;
  end Set_Double_Click_Delay;


  -- Put a whole field
  procedure Put_Fld (Field_No : in Field_Range;
                     State    : in State_List) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Char_Index  : Afpx_Typ.Char_Str_Range;
    Foreground  : Con_Io.Effective_Colors;
    Background  : Con_Io.Effective_Colors;
  begin
    -- Set colors
    Set_Colors (Field, State, Foreground, Background);

    -- Set index to start of field's data and to first char to put
    Char_Index := Field.Char_Index + Field.Offset;
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Af_Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      -- Put Field.Width chars, starting at Char_Index + Field.Offset
      Af_Con_Io.Putu (
         S          => Af_Dscr.Chars(Char_Index ..
                                     Char_Index + Field.Width - 1),
         Foreground => Foreground,
         Background => Background,
         Move       => False);
      -- Update Char_Index to first char of next row (except after last row)
      if I /= Field.Height then
        Char_Index := Char_Index + Field.Data_Len;
      end if;
    end loop;
  end Put_Fld;

  -- Erase a field (screen_background, screen_background)
  procedure Erase_Fld (Field_No : in Absolute_Field_Range) is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Spaces : constant Unicode_Sequence (1 .. Field.Width)
           := (others => Con_Io.Space);
  begin
    -- Put spaces in each row
    for I in 1 .. Field.Height loop
      -- Go to row, left of field
      Af_Con_Io.Move (Field.Upper_Left.Row + I - 1, Field.Upper_Left.Col);
      Af_Con_Io.Putu (S          => Spaces,
                      Foreground => Af_Con_Io.Get_Background,
                      Background => Af_Con_Io.Get_Background,
                      Move       => False);
    end loop;
  end Erase_Fld;

  -- Col after last non space character of a field (or last col)
  function Last_Col (Field_No :  Field_Range)
           return Con_Io.Col_Range is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields(Field_No);
    Str : constant Unicode_Sequence (1 .. Field.Data_Len)
        := Af_Dscr.Chars
              (Field.Char_Index .. Field.Char_Index + Field.Data_Len - 1);
  begin
    return Last_Index (Str, True);
  end Last_Col;

  -- Click is valid if Left or Right button press
  -- Wheele and Middle button press are handled internally here
  function Valid_Click (List_Present : Boolean;
                        Cursor_Field : Absolute_Field_Range;
                        Right_Select : Boolean;
                        List_Change_Cb : access
        procedure (Action : in List_Change_List;
                   Status : in List_Status_Rec) := null) return Boolean is

    Mouse_Status : Con_Io.Mouse_Event_Rec;
    Click_Pos : Con_Io.Square;
    Valid : Boolean;
    Scroll : Boolean;
    use type Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List,
             Afpx_Typ.Field_Kind_List;
  begin
    -- Check if mouse button is clicked
    Console.Get_Mouse_Event (Mouse_Status);
    Click_Pos := (Mouse_Status.Row, Mouse_Status.Col);
    -- We expect a click
    if Mouse_Status.Status /= Con_Io.Pressed then
      return False;
    end if;
    -- Scroll condition
    Scroll := List_Present and then In_Field_Absolute(Lfn, Click_Pos);
    case Mouse_Status.Button is
      when Con_Io.Left  =>
        Scroll := False;
        Valid := True;
      when Con_Io.Right =>
        Scroll := False;
        Valid := Right_Select;
      when Con_Io.Middle =>
        Scroll := False;
        -- At least one active Get field
        Valid := Af_Dscr.Fields(Cursor_Field).Kind = Get_Field
                 and then Af_Dscr.Fields(Cursor_Field).Activated
                 and then not Af_Dscr.Fields(Cursor_Field).Isprotected;
      when Con_Io.Up =>
        Scroll := Scroll and then Af_List.Update (Up, True);
        Valid := False;
      when Con_Io.Down =>
        Scroll := Scroll and then Af_List.Update (Down, True);
        Valid := False;
      when Con_Io.Shift_Up | Con_Io.Pgup =>
        Scroll := Scroll and then Af_List.Update (Page_Up, True);
        Valid := False;
      when Con_Io.Shift_Down | Con_Io.Pgdown =>
        Scroll := Scroll and then Af_List.Update (Page_Down, True);
        Valid := False;
      when Con_Io.Ctrl_Up | Con_Io.Shift_Pgup =>
        Scroll := Scroll and then Af_List.Update (Shift_Page_Up, True);
        Valid := False;
      when Con_Io.Ctrl_Down | Con_Io.Shift_Pgdown =>
        Scroll := Scroll and then Af_List.Update (Shift_Page_Down, True);
        Valid := False;
      when Con_Io.Ctrl_Pgup =>
        Scroll := Scroll and then Af_List.Update (Top, True);
        Valid := False;
      when Con_Io.Ctrl_Pgdown =>
        Scroll := Scroll and then Af_List.Update (Bottom, True);
        Valid := False;
      when others =>
        Scroll := False;
        Valid := False;
    end case;
    if Valid then
      Last_But := Mouse_Status.Button;
      Last_Pos := Click_Pos;
    end if;
    if Scroll and then List_Change_Cb /= null then
      List_Change_Cb (Afpx.Scroll, Af_List.Get_Status);
    end if;
    return Valid;
  end Valid_Click;

  -- Wait until release and return absolute pos
  function Wait_Release (Button : Button_List) return Con_Io.Square is
    Str : Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos : Positive;
    Ins : Boolean;
    Mouse_Status : Con_Io.Mouse_Event_Rec;
    use type Con_Io.Curs_Mvt,
             Con_Io.Mouse_Button_List, Con_Io.Mouse_Button_Status_List;
  begin
    -- Improve chances for the reversed button to be seen
    Console.Flush;
    delay Show_Click_Delay;
    -- Wait until button released
    loop
      Af_Con_Io.Get (Str, Last, Stat, Pos, Ins, Echo => False);
      if Stat = Con_Io.Mouse_Button then
        Console.Get_Mouse_Event (Mouse_Status);
        exit when ( (Button = Con_Io.Left
              and then Mouse_Status.Button = Con_Io.Left)
            or else (Button = Con_Io.Middle
              and then Mouse_Status.Button = Con_Io.Middle)
            or else (Button = Con_Io.Right
              and then Mouse_Status.Button = Con_Io.Right) )
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
                          Cursor_Field : in Absolute_Field_Range;
                          Right_Select : in Boolean;
                          Result : out Mouse_Action_Rec;
                          List_Change_Cb : access
        procedure (Action : in List_Change_List;
                   Status : in List_Status_Rec) := null) is
    Cursor_Pos : constant Con_Io.Square := Af_Con_Io.Position;
    Valid_Field : Boolean;
    Request_Selection : Boolean;
    Click_Pos : Con_Io.Square;
    Click_But : Button_List;
    List_But : List_Button_List;
    Click_Field : Absolute_Field_Range;
    Release_Pos : Con_Io.Square;
    Click_Row_List : Con_Io.Row_Range;
    Click_On_Selected : Boolean;
    Field : Afpx_Typ.Field_Rec;
    List_Status : List_Status_Rec;
    Click_Time : Ada.Calendar.Time;
    Loc_Last_Selected_Id : Natural;
    Selection_Modified : Boolean;

    List_Pos : Positive;
    List_Mod : Boolean;
    procedure Save_Pos is
    begin
      List_Mod := Line_List.Is_Modified;
      List_Pos := Line_List.Get_Position;
    end Save_Pos;
    procedure Restore_Pos is
    begin
      Line_List.Move_At (List_Pos);
      if not List_Mod then
        -- Not seen... not caught
        Line_List.Modification_Ack;
      end if;
    end Restore_Pos;
    function Get_Relative (Pos : Con_Io.Square) return Con_Io.Square is
      ((Row => Pos.Row - Field.Upper_Left.Row,
        Col => Pos.Col - Field.Upper_Left.Col));

    use type Ada.Calendar.Time;
    use type Button_List, Absolute_Field_Range, Field_Kind_List;
  begin
    -- Save and reset last selected id
    Loc_Last_Selected_Id := Last_Selected_Id;
    Last_Selected_Id := 0;

    -- Result event discarded
    Result := (Kind => Put_Field);
    if not Valid_Click (List_Present, Cursor_Field, Right_Select,
                        List_Change_Cb) then
      return;
    end if;
    Valid_Field := True;
    Request_Selection := False;
    -- Get button and pos, find field
    Click_But := Last_But;
    Click_Pos := Last_Pos;
    Click_Time := Ada.Calendar.Clock;
    if List_Present and then In_Field_Absolute(Lfn, Click_Pos)
    and then not Af_Dscr.Fields(Lfn).Isprotected
    and then Click_But /= Con_Io.Middle then
      List_But := (if Click_But = Con_Io.Left then List_Left else List_Right);
      -- Left/Right Selection in list
      Click_Field := Lfn;
      Click_Row_List := Click_Pos.Row - Af_Dscr.Fields(Lfn).Upper_Left.Row;
      List_Status := Af_List.Get_Status;
      -- Check that an item is displayed at this row
      if Click_Row_List >= List_Status.Nb_Rows then
        -- No data in this row
        Valid_Field := False;
      end if;
      -- Click on already selected item?
      if List_Status.Ids_Selected(List_But) /= 0
      and then Af_List.Id_Displayed (List_Status.Ids_Selected(List_But)) then
        Click_On_Selected :=
          Click_Row_List = Af_List.To_Row (List_Status.Ids_Selected(List_But));
      else
        Click_On_Selected := False;
      end if;
    elsif Click_But = Con_Io.Left then
      -- Try to find a button or get field
      Click_Field := 0;
      for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
        if Af_Dscr.Fields(I).Kind /= Put_Field and then
           Af_Dscr.Fields(I).Activated         and then
           not Af_Dscr.Fields(I).Isprotected   and then
           In_Field_Absolute(I, Click_Pos) then
          Click_Field := I;
          exit;
        end if;
      end loop;
      if Click_Field = 0 then
        -- Invalid field
        Valid_Field := False;
      end if;
    elsif Click_But = Con_Io.Middle then
      -- Try to find a get field
      Click_Field := 0;
      for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
        if Af_Dscr.Fields(I).Kind = Get_Field and then
           Af_Dscr.Fields(I).Activated        and then
           not Af_Dscr.Fields(I).Isprotected  and then
           In_Field_Absolute(I, Click_Pos) then
          Click_Field := I;
          exit;
        end if;
      end loop;
      if Click_Field = 0 then
        -- Invalid field
        Valid_Field := False;
      end if;
      -- Request selection anyway, after release
      Request_Selection := True;
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
        -- Reverse this Button/Get field
        Put_Fld (Click_Field, Clicked);
      end if;
    end if;

    -- Wait release. No keyboard input
    Release_Pos := Wait_Release (Click_But);

    -- Request selection just after receiving release
    if Request_Selection then
      Console.Request_Selection;
    end if;
    -- Done if click/release not valid
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
          if Click_But = Con_Io.Left then
            -- Left click on left selection then release outside:
            -- Restore left selection
            Af_List.Put (Click_Row_List, Selected, False);
          else
            -- Right click on right selection then release outside:
            -- Restore right selection
            Af_List.Put (Click_Row_List, Clicked, False);
          end if;
          Restore_Pos;
        elsif Click_But = Con_Io.Left
        and then Af_List.To_Id(Click_Row_List) = Loc_Last_Selected_Id
        and then Last_Selection_Time >= Click_Time - Double_Click_Delay then
          -- Double Left click
          Af_List.Put (Click_Row_List, Selected, False);
          Result := (Kind => Button_Field, But_Field_No => Click_Field,
                     But_Click_Square => Get_Relative (Click_Pos),
                     But_Release_Square => Get_Relative (Release_Pos));
        elsif Click_But = Con_Io.Left then
          -- Valid Left click. Store for next click to check double click
          Af_List.Put (Click_Row_List, Selected, False);
          List_Status := Af_List.Get_Status;
          Last_Selected_Id := List_Status.Ids_Selected(List_Left);
          Last_Selection_Time := Click_Time;
          if List_Change_Cb /= null then
            List_Change_Cb (Left_Selection, Af_List.Get_Status);
          end if;
        else
          -- Valid right click on selected: flip/flop => Unselect
          Af_List.Set_Selected (List_But, 0);
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
          -- Invalid release, restore clicked field
          Save_Pos;
          if Af_List.To_Id(Click_Row_List)
              = List_Status.Ids_Selected(List_Left) then
            -- Right click on left selection then release outside:
            -- Restore left selection
            Af_List.Put (Click_Row_List, Selected, False);
          elsif Af_List.To_Id(Click_Row_List)
              = List_Status.Ids_Selected(List_Right) then
            -- Left click on right selection then release outside:
            -- Restore right selection
            Af_List.Put (Click_Row_List, Clicked, False);
          else
            -- All other situations: cancel selection
            Af_List.Put (Click_Row_List, Normal, False);
          end if;
          Restore_Pos;
        else
          -- Valid release
          Selection_Modified := True;
          -- Un-select previous if it was shown
          if List_Status.Ids_Selected(List_But) /= 0
          and then Af_List.Id_Displayed (List_Status.Ids_Selected(List_But))
          then
            Save_Pos;
            Af_List.Put (Af_List.To_Row(List_Status.Ids_Selected(List_But)),
                         Normal, False);
            Restore_Pos;
          end if;
          if Click_But = Con_Io.Left then
            -- Left click
            if Af_List.To_Id(Click_Row_List)
             = List_Status.Ids_Selected(List_Right) then
              -- Left click on right selection:
              -- Reset Right selected
              Af_List.Set_Selected (List_Right, 0);
            end if;
            -- Set Left selected
            Af_List.Put (Click_Row_List, Selected, False);
            Af_List.Set_Selected (List_But, Af_List.To_Id(Click_Row_List));
          elsif Af_List.To_Id(Click_Row_List)
                /= List_Status.Ids_Selected(List_Left) then
            -- Right click on non selected:
            -- Set it right selected
            Af_List.Put (Click_Row_List, Clicked, False);
            Af_List.Set_Selected (List_But, Af_List.To_Id(Click_Row_List));
          else
            -- Right click on left selection: no right selection
            if List_Status.Ids_Selected(List_But) = 0 then
              -- Not cancelling a right selection: Avoid wrong notification
              Selection_Modified := False;
            else
              -- Cancel right selection
              Af_List.Set_Selected (List_But, 0);
            end if;
            -- Restore left selection
            Af_List.Put (Click_Row_List, Selected, False);
          end if;
          Af_List.Set_Current;
          if Selection_Modified and then List_Change_Cb /= null then
            if Click_But = Con_Io.Left then
              List_Change_Cb (Left_Selection, Af_List.Get_Status);
            else
              List_Change_Cb (Right_Selection, Af_List.Get_Status);
            end if;
          end if;
        end if;
        if Click_But = Con_Io.Left then
          -- Valid Left click. Store for next click to check double click
          List_Status := Af_List.Get_Status;
          Last_Selected_Id := List_Status.Ids_Selected (List_Left);
          Last_Selection_Time := Click_Time;
        end if;
      end if;
      -- Result is Put
      -- End of processing of click in list

    elsif Field.Kind = Get_Field then
      -- If field is get: restore color
      Put_Fld (Click_Field, Normal);
      if not Valid_Field
      or else (Click_But = Con_Io.Middle
               and then Click_Field = Cursor_Field)  then
        -- Invalid click/Release, or Middle button in current cursor field
        --  => Discard: keep cursor field and col
        Result := (Kind => Put_Field);
      else
        Result := (Kind => Get_Field,
                   Get_Field_No => Click_Field,
                   Click_Col => Click_Pos.Col
                    - Af_Dscr.Fields(Click_Field).Upper_Left.Col);
      end if;
    else
      -- If field is button: restore color
      Put_Fld (Click_Field, Normal);
      if Valid_Field then
        Result := (Kind => Button_Field, But_Field_No => Click_Field,
                   But_Click_Square => Get_Relative (Click_Pos),
                   But_Release_Square => Get_Relative (Release_Pos));
      end if;
    end if;

    Af_Con_Io.Move (Cursor_Pos);

  end Handle_Click;

  -- Get and paste selection (if any) in Selection_Field
  --  and at Selection_Col
  -- Return -2 if selection failed (no change)
  -- Return -1 if end of field reached
  -- Otherwise return the new cursor col
  Sel_No_Change : constant Integer := -2;
  Sel_New_Field : constant Integer := -1;
  function Handle_Selection (Handle : Get_Handle_Rec) return Integer is
    -- The selection retrived from the Console
    Sel_Txt : As.U.Asu_Us;
    Sel_Len : Natural;
    -- Cursor (Get) field and its initial content
    Field : constant Afpx_Typ.Field_Rec
          := Af_Dscr.Fields(Handle.Cursor_Field);
    Column : constant  Con_Io.Col_Range := Handle.Cursor_Col;
    Index  : constant Afpx_Typ.Char_Str_Range := Field.Offset + Column + 1;
    Offset : Con_Io.Col_Range;
    Str : Unicode.Unbounded_Unicode.Unbounded_Array;
    Str_Len : Natural;
    Result : Integer;
  begin
     -- Get selection and replace Line_Feed by Space
    Sel_Txt := As.U.Tus (Console.Get_Selection (Width_Range'Last));
    if Sel_Txt.Length = 0 then
      -- No selection
      return Sel_No_Change;
    end if;
    for I  in 1 .. Sel_Txt.Length loop
      if Sel_Txt.Element (I) = Aski.Lf then
        Sel_Txt.Replace_Element (I, ' ');
      end if;
    end loop;

    -- Insert/Overwrite the selection in Str
    declare
      Selection : constant Unicode_Sequence
                := Language.String_To_Unicode (Sel_Txt.Image);
    begin
      Sel_Len := Selection'Length;
      -- Current content of field from cursor (included) to end
      Str.Set (Af_Dscr.Chars(Field.Char_Index + Field.Offset + Column
                          .. Field.Char_Index + Field.Data_Len - 1));
      Str_Len := Str.Length;
      if Sel_Len = 0 then
        return Sel_No_Change;
      end if;
      -- New content inserted before / overwritting from current pos
      if Handle.Insert then
        Str.Insert (1, Selection);
      else
        Str.Overwrite (1, Selection);
      end if;
    end;
    -- Trunk at end of data_len, Str must be remain of length Str_Len
    if Str.Length > Str_Len then
      Str.Delete (Str_Len + 1, Str.Length);
    end if;

    -- Compute new Offset and Cursor_Col (Result)
    -- Index is initial relative index in Chars(1 .. Data_Len)
    if Index + Sel_Len >= Field.Data_Len then
      -- Cursor would move out of field
      if Field.Move_Next then
        Result := Sel_New_Field;
      else
        -- Last column of field
        Result := Field.Width - 1;
      end if;
      -- Max offset
      Offset := Field.Data_Len - Field.Width;
    else
      -- Cursor remains within field data
      Result := Column + Sel_Len;
      if Result > Field.Width - 1 then
        -- Scroll, cursor moves at the right side of the window
        Offset := Field.Offset + Result - Field.Width + 1;
        Result := Field.Width - 1;
      else
        -- No scroll, move cursor
        Offset := Field.Offset;
      end if;
    end if;

    -- 'Encode' new content
    Af_Dscr.Chars(Field.Char_Index + Field.Offset + Column
               .. Field.Char_Index + Field.Data_Len - 1) := Str.To_Array;
    Af_Dscr.Current_Dscr.Modified := True;
    -- Update offset
    Af_Dscr.Fields(Handle.Cursor_Field).Offset := Offset;

    return Result;
  exception
    when others =>
      return Sel_No_Change;
  end Handle_Selection;

  -- Call the user callback to get cursor col
  function Get_Cursor_Col (
                 Field_No : Field_Range;
                 New_Field : Boolean;
                 Pointer_Col : Con_Io.Col_Range;
                 Offset : Con_Io.Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Cursor_Col_Cb : access
    function (Cursor_Field : Field_Range;
              New_Field : Boolean;
              Pointer_Col : Con_Io.Col_Range;
              Offset : Con_Io.Col_Range;
              Enter_Field_Cause : Enter_Field_Cause_List;
              Str : Unicode_Sequence) return Con_Io.Col_Range)
  return Con_Io.Col_Range is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields (Field_No);
    Result : Con_Io.Col_Range;
    Signif_Col : Con_Io.Col_Range;
  begin
    -- Call Cb if set
    if Cursor_Col_Cb = null then
      case Enter_Field_Cause is
        when Right_Full | Tab | Stab =>
          -- Start of field if Right_Full, Tab or Stab
          return Con_Io.Col_Range'First;
        when Left =>
          -- Last significant col if Left
          Signif_Col := Last_Col (Field_No);
          return Signif_Col;
        when Mouse =>
          -- When click in a Get, set cursor where clicked if there is a
          --  significant char there or on its right, otherwise set it just
          --  after last significant char
          Signif_Col := Last_Col (Field_No);
          if Pointer_Col > Signif_Col then
            return Signif_Col;
          else
            return Pointer_Col;
          end if;
      end case;
    end if;
    -- The user Cb will appreciate a string (1 .. Len)
    --  so make a local copy
    declare
      Str : constant Unicode_Sequence (1 .. Field.Data_Len)
          := Af_Dscr.Chars
              (Field.Char_Index .. Field.Char_Index + Field.Data_Len - 1);
    begin
      Result := Cursor_Col_Cb (Field_No,
                               New_Field,
                               Pointer_Col,
                               Offset,
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

  function Significant_Char (Field_No : Field_Range;
                             From_Head : Boolean)
           return Con_Io.Col_Range is
    Field : constant Afpx_Typ.Field_Rec := Af_Dscr.Fields (Field_No);
    Str : constant Unicode_Sequence
        := Af_Dscr.Chars (Field.Char_Index ..
                          Field.Char_Index + Field.Data_Len - 1);
  begin
    if From_Head then
      return First_Index (Str, True);
    else
      return Last_Index (Str, True);
    end if;
  end Significant_Char;

  -- Force redisplay at next Ptg
  procedure Redisplay is
  begin
    Need_Redisplay := True;
    Prev_Cursor_Field := List_Field_No;
  end Redisplay;

  -- Print the fields and the list, then gets
  procedure Ptg (Get_Handle    : in out Get_Handle_Rec;
                 Result        : out Result_Rec;
                 Right_Select  : in Boolean;
                 Get_Active    : in Boolean;

                 Cursor_Col_Cb : access
     function (Cursor_Field : Field_Range;
               New_Field : Boolean;
               Pointer_Col : Con_Io.Col_Range;
               Offset : Con_Io.Col_Range;
               Enter_Field_Cause : Enter_Field_Cause_List;
               Str : Unicode_Sequence)
               return Con_Io.Col_Range := null;

                 List_Change_Cb : access
     procedure (Action : in List_Change_List;
                Status : in List_Status_Rec) := null) is

    Cursor_Field : Absolute_Field_Range;
    List_Present : Boolean;
    Field : Afpx_Typ.Field_Rec;
    Last : Natural;
    Stat : Con_Io.Extra_Mvt;
    Pos : Positive;
    Foreground : Con_Io.Effective_Colors;
    Background : Con_Io.Effective_Colors;
    Done : Boolean;
    Selection_Result : Integer;
    List_Init : Boolean;
    List_Scrolled : Boolean;
    Field_Start : Afpx_Typ.Char_Str_Range;
    Change : Boolean;
    Char : Unicode_Number;
    Index : Natural;

    use type Con_Io.Curs_Mvt, Afpx.Absolute_Field_Range;

  begin
    -- For callback of list init
    List_Init := False;

    -- Reset last selection for double click
    Last_Selected_Id := 0;

    -- Erase potential garbage and set Dscr background if redisplay
    if Need_Redisplay then
      Af_Con_Io.Clear;
    end if;

    -- Init Cursor field and its offset
    if Get_Active then
      Cursor_Field := Get_Handle.Cursor_Field;
      Af_Dscr.Fields(Cursor_Field).Offset := Get_Handle.Offset;
    else
      Cursor_Field := List_Field_No;
      Prev_Cursor_Field := Afpx_Typ.List_Field_No;
    end if;

    -- The infinite loop
    loop

      -- List present : defined, activated and not empty
      List_Present := Af_Dscr.Has_List
             and then Af_Dscr.Fields(Lfn).Activated
             and then not Line_List.Is_Empty;

      -- Update list if Line_List has changed
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
      elsif Af_Dscr.Has_List then
        -- List exists but is empty or inactive
        Af_List.Set_Selected (List_Left, 0);
        Af_List.Set_Selected (List_Right, 0);
      end if;

      -- Force List to be updated if Line_List has changed
      if Line_List.Is_Modified then
        Af_Dscr.Fields(Lfn).Modified := True;
        Line_List.Modification_Ack;
      end if;

      -- Update list
      if Af_Dscr.Has_List
      and then (Need_Redisplay or else Af_Dscr.Fields(Lfn).Modified) then
        if not Af_Dscr.Has_List then
          -- No list in this Dscr
          null;
        elsif not Af_Dscr.Fields(Lfn).Activated then
          -- List not active
          Erase_Fld (Lfn);
        elsif Line_List.Is_Empty then
          -- Empty list
          Af_List.Display(1);
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
      end if;

      -- Call List_Scrolled_Cb only once, when called
      if Af_Dscr.Has_List and then List_Change_Cb /= null
      and then not List_Init then
        List_Change_Cb (Init, Af_List.Get_Status);
      end if;
      List_Init := True;

      -- Redisplay all fields if requested, or modified fields, or
      --  cursor field that has changed
      if Need_Redisplay
      or else Af_Dscr.Current_Dscr.Modified
      or else Cursor_Field /= Prev_Cursor_Field then
        for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
          if Need_Redisplay
          or else Af_Dscr.Fields(I).Modified
          or else (Cursor_Field /= Prev_Cursor_Field
            and then (I = Cursor_Field or else I = Prev_Cursor_Field) ) then
            Af_Dscr.Fields(I).Modified := False;
            if Af_Dscr.Fields(I).Activated then
              Put_Fld (I, Normal);
            else
              Erase_Fld (I);
            end if;
          end if;
        end loop;
      end if;
      Prev_Cursor_Field := Cursor_Field;

      -- Get field, set colors when field changes
      if Get_Active then
        Field := Af_Dscr.Fields(Cursor_Field);
        Set_Colors (Field, Selected, Foreground, Background);
      end if;

      -- Flush output
      Console.Flush;

      -- No more forced redisplay and/or flush
      Need_Redisplay := False;
      Af_Dscr.Current_Dscr.Modified := False;
      Af_Dscr.Fields(Lfn).Modified := False;

      if Get_Active then
        Field_Start := Field.Char_Index + Field.Offset;
        Pos := Get_Handle.Cursor_Col + 1;
        -- Move at beginning of field and put_then_get
        Af_Con_Io.Move (Field.Upper_Left.Row, Field.Upper_Left.Col);
        Char := Af_Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars(Field_Start .. Field_Start + Field.Width - 1),
         Extra => Field.Data_Len /= Field.Width,
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Get_Handle.Insert,
         Foreground => Foreground,
         Background => Background);
        Get_Handle.Cursor_Col := Pos - 1;
      else
        -- Blind get
        Get_Handle.Insert := False;
        Af_Con_Io.Move (Af_Con_Io.Row_Range_Last, Af_Con_Io.Col_Range_Last);
        Char := Af_Con_Io.Put_Then_Get (
         Str    => Af_Dscr.Chars (1 .. 0),
         Extra => False,
         Last   => Last,
         Stat   => Stat,
         Pos    => Pos,
         Insert => Get_Handle.Insert,
         Foreground => Af_Con_Io.Get_Background,
         Background => Af_Con_Io.Get_Background);
       end if;

      Done := False;
      -- Now the big case on keys
      List_Scrolled := False;
      case Stat is
        -- Extra Con_Io codes returned to preserve a scrolling Get field
        when Con_Io.Backspace =>
          if Get_Active then
            if Pos = 1 then
              -- Cursor is at Leftmost column of field
              if Field.Offset = Afpx_Typ.Offset_Range'First then
                -- Fully at beginning of string => nothing happens
                Change := False;
              else
                -- Shift string right
                Field.Offset := Field.Offset - 1;
                Change := True;
              end if;
            else
              Pos := Pos - 1;
              Change := True;
            end if;
            if Change then
              Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
              Get_Handle.Cursor_Col := Pos - 1;
              -- Overwrite char at Pos, by scrolling left and append space
              Af_Dscr.Chars(Field_Start + Pos - 1
                         .. Field.Char_Index + Field.Data_Len - 2) :=
                Af_Dscr.Chars(Field_Start + Pos
                           .. Field.Char_Index + Field.Data_Len - 1);
              Af_Dscr.Chars(Field.Char_Index + Field.Data_Len - 1) :=
                Con_Io.Space;
            end if;
          end if;
        when Con_Io.Suppr =>
          if Get_Active then
            -- Overwrite char at Pos, by scrolling left and append space
            Af_Dscr.Chars(Field_Start + Pos - 1
                       .. Field.Char_Index + Field.Data_Len - 2) :=
              Af_Dscr.Chars(Field_Start + Pos
                         .. Field.Char_Index + Field.Data_Len - 1);
            Af_Dscr.Chars(Field.Char_Index + Field.Data_Len - 1) :=
              Con_Io.Space;
          end if;
        when Con_Io.Ctrl_Suppr =>
          if Get_Active then
            -- Clear the String and move to beginning
            Field.Offset := Con_Io.Col_Range_First;
            Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
            Get_Handle.Cursor_Col := Con_Io.Col_Range_First;
            Af_Dscr.Chars(Field.Char_Index
                       .. Field.Char_Index + Field.Data_Len - 1) :=
                (others => Con_Io.Space);
          end if;
        when Con_Io.Shift_Suppr =>
          if Get_Active then
            -- Clear from current (included) to end of string
            Af_Dscr.Chars(Field_Start + Pos - 1
                       .. Field.Char_Index + Field.Data_Len - 1) :=
                (others => Con_Io.Space);
          end if;
        when Con_Io.Insert =>
          if Get_Active then
            -- Shift right and insert char
            Af_Dscr.Chars(Field_Start + Pos
                       .. Field.Char_Index + Field.Data_Len - 1) :=
              Af_Dscr.Chars(Field_Start + Pos - 1
                         .. Field.Char_Index + Field.Data_Len - 2);
            Af_Dscr.Chars(Field_Start + Pos - 1) := Char;
            if Field.Offset + Pos /= Field.Data_Len then
              -- Move cursor right, or scroll left
              if Pos = Field.Width then
                -- we are at rightmost pos => Scroll left
                Field.Offset := Field.Offset + 1;
                Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
              else
                -- We can move right
                Pos := Pos + 1;
                Get_Handle.Cursor_Col := Pos - 1;
              end if;
            end if;
          end if;
        -- Normal Con_Io Cursor movements
        when Con_Io.Up =>
          -- List scroll down
          if List_Present then
            List_Scrolled := Af_List.Update (Up, True);
          end if;
        when Con_Io.Down =>
          -- List scroll up
          if List_Present then
            List_Scrolled := Af_List.Update (Down, True);
          end if;
        when Con_Io.Shift_Up | Con_Io.Pgup =>
          -- List page up
          if List_Present then
            List_Scrolled := Af_List.Update (Page_Up, True);
          end if;
        when Con_Io.Shift_Down | Con_Io.Pgdown =>
          -- List page down
          if List_Present then
            List_Scrolled := Af_List.Update (Page_Down, True);
          end if;
        when Con_Io.Ctrl_Up | Con_Io.Shift_Pgup =>
          -- List several pages up
          if List_Present then
            List_Scrolled := Af_List.Update (Shift_Page_Up, True);
          end if;
        when Con_Io.Ctrl_Down | Con_Io.Shift_Pgdown =>
          -- List several pages down
          if List_Present then
            List_Scrolled := Af_List.Update (Shift_Page_Down, True);
          end if;
        when Con_Io.Ctrl_Pgup =>
          -- List top
          if List_Present then
            List_Scrolled := Af_List.Update (Top, True);
          end if;
        when Con_Io.Ctrl_Pgdown =>
          -- List bottom
          if List_Present then
            List_Scrolled := Af_List.Update (Bottom, True);
          end if;
        when Con_Io.Right | Con_Io.Full =>
          if Get_Active then
            if Field.Offset + Field.Width < Field.Data_Len then
              -- Offset + Width must remain <= Data_Len
              Field.Offset := Field.Offset + 1;
              Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
            elsif Field.Move_Next then
              -- End (right) of previous field
              -- Restore normal color of previous field
              Put_Fld (Cursor_Field, Normal);
              Cursor_Field := Next_Get_Field (Cursor_Field);
              Get_Handle.Offset := Con_Io.Col_Range'First;
              Af_Dscr.Fields(Cursor_Field).Offset := Get_Handle.Offset;
              Get_Handle.Cursor_Col := Get_Cursor_Col (
                  Cursor_Field,
                  True,
                  Con_Io.Col_Range'First,
                  Get_Handle.Offset,
                  Right_Full, Cursor_Col_Cb);
              Get_Handle.Insert := False;
            end if;
          end if;
        when Con_Io.Left =>
          if Get_Active then
            if Field.Offset /= 0 then
              Field.Offset := Field.Offset - 1;
              Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
            elsif Field.Move_Prev then
              -- Left on previous field
              -- Restore normal color of previous field
              Put_Fld (Cursor_Field, Normal);
              Cursor_Field := Prev_Get_Field (Cursor_Field);
              Field := Af_Dscr.Fields(Cursor_Field);
              Field.Offset := Field.Data_Len - Field.Width;
              Get_Handle.Offset := Field.Offset;
              Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
              Get_Handle.Cursor_Col := Get_Cursor_Col (
                  Cursor_Field,
                  True,
                  Con_Io.Col_Range'First,
                  Get_Handle.Offset,
                  Left, Cursor_Col_Cb);
              -- The returned cursor col is somewhere in Field(1 .. Data_len)
              -- Adapt offset to have it at pos Width: first slot is Width-1
              if Get_Handle.Cursor_Col >= Field.Width - 1 then
                -- Yes we can
                Field.Offset := Get_Handle.Cursor_Col - Field.Width + 1;
                Get_Handle.Cursor_Col := Field.Width - 1;
              else
                -- Nop, show head and move Pos
                Field.Offset := Con_Io.Col_Range_First;
              end if;
              Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
              Get_Handle.Insert := False;
            end if;
          end if;
        when Con_Io.Ctrl_Left =>
          if Get_Active then
            -- 1 before first significant char
            Index := Significant_Char (Cursor_Field, True);
            -- Try to have it at pos 1: last slot is Data_Len-Width
            if Index <= Field.Data_Len - Field.Width then
              -- Yes we can
              Field.Offset := Index;
              Get_Handle.Cursor_Col := Con_Io.Col_Range_First;
            else
              -- Nop, show tail and move Pos
              Field.Offset := Field.Data_Len - Field.Width;
              Get_Handle.Cursor_Col := Field.Width - (Field.Data_Len - Index);
            end if;
            Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
          end if;
        when Con_Io.Ctrl_Right =>
          if Get_Active then
            -- 1 after last significant char
            Index := Significant_Char (Cursor_Field, False);
            -- Try to have it at pos Width: first slot is Width-1
            if Index >= Field.Width - 1 then
              -- Yes we can
              Field.Offset := Index - Field.Width + 1;
              Get_Handle.Cursor_Col := Field.Width - 1;
            else
              -- Nop, show head and move Pos
              Field.Offset := Con_Io.Col_Range_First;
              Get_Handle.Cursor_Col := Index;
            end if;
            Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
          end if;
        when Con_Io.Shift_Left =>
          if Get_Active then
            -- Beginning of data
            Field.Offset := Con_Io.Col_Range'First;
            Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
            Get_Handle.Cursor_Col := Con_Io.Col_Range_First;
          end if;
        when Con_Io.Shift_Right =>
          if Get_Active then
            -- End of data
            Field.Offset := Field.Data_Len - Field.Width;
            Af_Dscr.Fields(Cursor_Field).Offset := Field.Offset;
            Get_Handle.Cursor_Col := Field.Width - 1;
          end if;
        when Con_Io.Tab =>
          if Get_Active then
            -- Tab in previous field
            -- Restore normal color of previous field
            Put_Fld (Cursor_Field, Normal);
            Cursor_Field := Next_Get_Field (Cursor_Field);
            Get_Handle.Offset := Con_Io.Col_Range'First;
            Af_Dscr.Fields(Cursor_Field).Offset := Get_Handle.Offset;
            Get_Handle.Cursor_Col := Get_Cursor_Col (
                Cursor_Field,
                True,
                Con_Io.Col_Range'First,
                Get_Handle.Offset,
                Tab, Cursor_Col_Cb);
            Get_Handle.Insert := False;
          end if;
        when Con_Io.Stab =>
          if Get_Active then
            -- Beginning of prev get field
            -- Restore normal color of previous field
            Put_Fld (Cursor_Field, Normal);
            Cursor_Field := Prev_Get_Field (Cursor_Field);
            Get_Handle.Offset := Con_Io.Col_Range'First;
            Af_Dscr.Fields(Cursor_Field).Offset := Get_Handle.Offset;
            Get_Handle.Cursor_Col := Get_Cursor_Col (
                Cursor_Field,
                True,
                Con_Io.Col_Range'First,
                Get_Handle.Offset,
                Stab, Cursor_Col_Cb);
            Get_Handle.Insert := False;
          end if;
        when Con_Io.Ret =>
          -- End put_then_get on keyboard ret
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Return_Key);
          Get_Handle.Insert := False;
          Done := True;
        when Con_Io.Esc =>
          -- End put_then_get on keyboard esc
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Escape_Key);
          Get_Handle.Insert := False;
          Done := True;
        when Con_Io.Selection =>
          -- Paste selection
          if Get_Active  then
            Selection_Result := Handle_Selection (Get_Handle);
            if Selection_Result = Sel_New_Field then
              -- Selection completed the field
              -- Restore normal color of previous field
              Put_Fld (Cursor_Field, Normal);
              Cursor_Field := Next_Get_Field (Cursor_Field);
              Get_Handle.Offset := Con_Io.Col_Range'First;
              Af_Dscr.Fields(Cursor_Field).Offset := Get_Handle.Offset;
              Get_Handle.Cursor_Col := Get_Cursor_Col (
                  Cursor_Field,
                  True,
                  Con_Io.Col_Range'First,
                  Get_Handle.Offset,
                  Right_Full, Cursor_Col_Cb);
              Get_Handle.Insert := False;
            elsif Selection_Result /= Sel_No_Change then
              -- Selection moved the cursor
              Get_Handle.Cursor_Col := Selection_Result;
              Get_Handle.Offset := Af_Dscr.Fields(Cursor_Field).Offset;
            end if;
          end if;
        when Con_Io.Mouse_Button =>
          declare
            Click_Result : Mouse_Action_Rec;
          begin
            Handle_Click (List_Present, Cursor_Field,
                          Right_Select, Click_Result, List_Change_Cb);
            case Click_Result.Kind is
              when Put_Field =>
                null;
              when Get_Field =>
                if Click_Result.Get_Field_No = Cursor_Field then
                  -- Same field, update cursor col
                  Get_Handle.Cursor_Col := Get_Cursor_Col (
                      Cursor_Field,
                      False,
                      Click_Result.Click_Col,
                      Af_Dscr.Fields(Cursor_Field).Offset,
                      Mouse, Cursor_Col_Cb);
                else
                  -- Restore normal color of previous field
                  Put_Fld (Cursor_Field, Normal);
                  -- Change field
                  Cursor_Field := Click_Result.Get_Field_No;
                  -- Update cursor col
                  Get_Handle.Cursor_Col := Get_Cursor_Col (
                      Cursor_Field,
                      True,
                      Click_Result.Click_Col,
                      Af_Dscr.Fields(Cursor_Field).Offset,
                      Mouse, Cursor_Col_Cb);
                  Get_Handle.Insert := False;
                end if;
              when Button_Field =>
                -- End of put_then_get
                if List_Present then
                  Af_List.Set_Current;
                end if;
                Result := (Id_Selected_Right  =>
                             Af_List.Get_Status.Ids_Selected (List_Right),
                           Event        => Mouse_Button,
                           Field_No     => Click_Result.But_Field_No,
                           Click_Pos    => Click_Result.But_Click_Square,
                           Release_Pos  => Click_Result.But_Release_Square);
                Get_Handle.Insert := False;
                Done := True;
            end case;
          end;
        when Con_Io.Break =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Keyboard,
                     Keyboard_Key => Break_Key);
          Get_Handle.Insert := False;
          Done := True;
        when Con_Io.Refresh =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Refresh);
          Need_Redisplay := True;
          Done := True;
        when Con_Io.Fd_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Fd_Event);
          Done := True;
        when Con_Io.Timer_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Timer_Event);
          Done := True;
        when Con_Io.Signal_Event =>
          if List_Present then
            Af_List.Set_Current;
          end if;
          Result := (Id_Selected_Right  =>
                       Af_List.Get_Status.Ids_Selected (List_Right),
                     Event        => Signal_Event);
          Done := True;
        when Con_Io.Timeout =>
          null;
      end case;

      if Get_Active then
        Get_Handle.Cursor_Field := Field_Range (Cursor_Field);
        Get_Handle.Offset := Af_Dscr.Fields(Cursor_Field).Offset;
      end if;

      -- Notify of change of list because of key
      if List_Scrolled and then List_Change_Cb /= null then
        List_Change_Cb (Scroll, Af_List.Get_Status);
      end if;

      exit when Done;
    end loop;

  end Ptg;

end Af_Ptg;

