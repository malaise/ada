with Afpx, Con_Io, Normal, Afpx_Xref, Trace.Loggers;
package body Screen is

  Logger : Trace.Loggers.Logger;

  -- Stick color
  Stick_Color : constant Con_Io.Effective_Colors
              := Con_Io.Color_Of ("Light_Grey");
  Sel_Color : constant Con_Io.Effective_Colors
            := Con_Io.Color_Of ("Red");

  function Intro return Common.Game_Kind_List is
    -- For put then get
    Get_Handle : Afpx.Get_Handle_Rec;
    Result : Afpx.Result_Rec;
    use type Afpx.Event_List, Afpx.Keyboard_Key_List, Afpx.Absolute_Field_Range;
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Intro.Dscr_Num);
    loop
      Afpx.Put_Then_Get (Get_Handle, Result);
      exit when Result.Event = Afpx.Mouse_Button;
      if Result.Event = Afpx.Signal_Event
      or else (Result.Event = Afpx.Keyboard
               and then Result.Keyboard_Key = Afpx.Break_Key) then
        raise Common.Exit_Requested;
      end if;
    end loop;
    if Result.Field_No = Afpx_Xref.Intro.Play_Nim then
      return Common.Nim;
    elsif Result.Field_No = Afpx_Xref.Intro.Play_Marienbad then
      return Common.Marienbad;
    elsif Result.Field_No =  Afpx_Xref.Intro.Quit then
      raise Common.Exit_Requested;
    end if;
    -- To avoid warning
    return Common.Nim;
  end Intro;

  procedure Update_Scores is
    Scores : constant Common.Score_Array := Common.Get_Scores;
  begin
    Afpx.Encode_Field (Afpx_Xref.Game.Names, (0,  1),
                  "You: " & Normal (Scores(Common.Human), 3));
    Afpx.Encode_Field (Afpx_Xref.Game.Names, (0, 13),
                  "Me: " & Normal (Scores(Common.Machine), 3));
  end Update_Scores;

  procedure Reset is
    use type Afpx.Descriptor_Range;
    use type Common.Game_Kind_List;
  begin
    Logger.Init ("Screen");
    Logger.Log_Debug ("Reset");
    if not Afpx.Is_Descriptor_Set
    or else Afpx.Get_Descriptor /= Afpx_Xref.Game.Dscr_Num then
      Afpx.Use_Descriptor (Afpx_Xref.Game.Dscr_Num);
    end if;
    Update_Scores;
    Afpx.Clear_Field (Afpx_Xref.Game.Game);
    Afpx.Clear_Field (Afpx_Xref.Game.Play);
    if Common.Get_Game_Kind = Common.Nim then
      Afpx.Encode_Field (Afpx_Xref.Game.Game, (0,0), "   Nim");
      Afpx.Encode_Field (Afpx_Xref.Game.Play, (1,1), "Play Marienbad");
    else
      Afpx.Encode_Field (Afpx_Xref.Game.Game, (0,0), "Marienbad");
      Afpx.Encode_Field (Afpx_Xref.Game.Play, (1,1), "   Play Nim");
    end if;
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Play, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Reset, False);
  end Reset;

  -- Init Bars activation
  procedure Init is
    Row_Col : Common.Row_Col_Rec;
    Indexes : Common.Indexes_Rec;
    Bars : Common.Bar_Status_Array;
  begin
    -- Init
    for I in Common.Row_Range loop
      Bars := Common.Get_Bars (I);
      Indexes := Common.Get_Indexes (I);
      for J in Indexes.First_Index .. Indexes.Last_Index loop
        Row_Col := Common.Index2Row_Col (J);
        Afpx.Reset_Field (J, Reset_String => False);
        Afpx.Set_Field_Activation (J, Bars(Row_Col.Col));
      end loop;
    end loop;
  end Init;

  procedure Play (Row : out Common.Row_Range;
                  Remove : out Common.Bar_Status_Array) is
    -- For put then get
    Get_Handle : Afpx.Get_Handle_Rec;
    Result : Afpx.Result_Rec;

    -- The current selection
    Selection_Index : Common.Index_Range;
    Nb_Selected : Natural;
    Row_Selected : Common.Row_Range;
    Row_Col : Common.Row_Col_Rec;
    Bars : Common.Bar_Status_Array;
    type Selected_Array is array (Common.Index_Range) of Boolean;
    Selected : Selected_Array := (others => False);

    -- (Un) select all the bars of row
    procedure Select_All (On : in Boolean) is
    begin
      for I in Bars'Range loop
        if Bars(I) then
          Selection_Index := Common.Row_Col2Index (
              (Row_Col.Row, I - Bars'First + 1) );
          Selected(Selection_Index) := On;
          Afpx.Set_Field_Colors (Selection_Index,
            Background => (if On then Sel_Color else Stick_Color) );
        end if;
      end loop;
    end Select_All;

    use type Afpx.Event_List, Afpx.Keyboard_Key_List;
  begin
    Logger.Log_Debug ("Play");
    -- Init
    Init;
    for I in Common.Index_Range loop
      Selected(I) := False;
    end loop;

    Nb_Selected := 0;
    Row_Selected := 1;
    loop
      -- Activate play
      Afpx.Set_Field_Activation (Afpx_Xref.Game.Remove, Nb_Selected /= 0);
      Afpx.Put_Then_Get (Get_Handle, Result);
      if Result.Event = Afpx.Signal_Event
      or else (Result.Event = Afpx.Keyboard
               and then Result.Keyboard_Key = Afpx.Break_Key) then
        Logger.Log_Debug ("Break");
        raise Common.Exit_Requested;
      end if;
      if Result.Event = Afpx.Mouse_Button then
        case Result.Field_No is
          when Common.Index_Range =>
            Selection_Index := Result.Field_No;
            Row_Col := Common.Index2Row_Col(Selection_Index);
            Logger.Log_Debug ("  Clicked Row:" & Row_Col.Row'Img
                            & "  Col:" & Row_Col.Col'Img);

            -- First selection
            if Nb_Selected = 0 then
              Row_Selected := Row_Col.Row;
            end if;
            Logger.Log_Debug ("  Row_Selected:" & Row_Selected'Img
                            & "  Nb_Selected:" & Nb_Selected'Img);

            -- Take selection / unselect
            if Row_Col.Row = Row_Selected then
              if Result.Double_Click then
                Logger.Log_Debug ("  Double click");
                Bars := Common.Get_Bars (Row_Selected);
                if Nb_Selected = Common.Nb_Bars (Bars) - 1
                and then not Selected(Selection_Index) then
                  -- Prev click has unselected current bar and all the
                  -- other bars of the row are selected, which means that
                  -- all the bars were selected => unselect all the bars
                  Logger.Log_Debug ("  Unselect all");
                  Select_All (False);
                  Nb_Selected := 0;
                else
                  -- Some bars are not selected => Select all the bars
                  Logger.Log_Debug ("  Select all");
                  Select_All (True);
                  Nb_Selected := Common.Nb_Bars (Bars);
                end if;
              elsif Selected(Selection_Index) then
                Logger.Log_Debug ("  Unselect one");
                Selected(Selection_Index) := False;
                Afpx.Set_Field_Colors (Result.Field_No,
                                       Background => Stick_Color);
                Nb_Selected := Nb_Selected - 1;
              else
                Logger.Log_Debug ("  Select one");
                Selected(Selection_Index) := True;
                Afpx.Set_Field_Colors (Result.Field_No,
                                       Background => Sel_Color);
                Nb_Selected := Nb_Selected + 1;
              end if;
            end if;
            Logger.Log_Debug ("  => Nb_Selected:" & Nb_Selected'Img);

          when Afpx_Xref.Game.Remove =>
            -- Take selection: set Row and Bars to remove
            Logger.Log_Debug ("  Remove selection");
            Row := Row_Selected;
            Remove := (others => False);
            for I in Selected'Range loop
              if Selected(I)  then
                Afpx.Set_Field_Activation (I, False);
                Remove(Common.Index2Row_Col (I).Col) := True;
              end if;
            end loop;
            exit;
          when Afpx_Xref.Game.Quit =>
            Logger.Log_Debug ("Quit");
            raise Common.Exit_Requested;
          when others =>
            null;
        end case;
      end if;
    end loop;

  end Play;

  procedure Update (Row : in Common.Row_Range;
                    Remove : in Common.Bar_Status_Array) is
    Cols : constant Common.Cols_Rec := Common.Get_Cols (Row);
  begin
    Logger.Log_Debug ("Update");
    -- Init
    Init;

    -- Remove bars (machine plays)
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Remove, False);
    for I in Cols.First_Col .. Cols.Last_Col loop
      if Remove (I) then
        Afpx.Set_Field_Colors (Common.Row_Col2Index ((Row, I)),
                               Background => Sel_Color);
        Afpx.Put;
        delay 0.1;
      end if;
    end loop;

    -- Display
    Afpx.Put;
    delay 0.4;

    -- Remove
    for I in Cols.First_Col .. Cols.Last_Col loop
      if Remove (I) then
        Afpx.Set_Field_Colors (Common.Row_Col2Index ((Row, I)),
                               Background => Sel_Color);
        Afpx.Set_Field_Activation (Common.Row_Col2Index ((Row, I)),
                                   False);
      end if;
    end loop;
  end Update;

  -- Show end of a game
  procedure End_Game (Result : in Common.Done_Result_List;
                      Change_Game : out Boolean) is
    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;
    use type Common.Played_Result_List;
    use type Afpx.Event_List, Afpx.Keyboard_Key_List, Afpx.Absolute_Field_Range;
  begin
    Logger.Log_Debug ("End game");
    for I in Common.Index_Range loop
      Afpx.Set_Field_Protection(I, True);
    end loop;
    -- Validate
    if Result = Common.Played_And_Won or else Result = Common.Won then
      Afpx.Encode_Field(Afpx_Xref.Game.Wins, (0, 34), "I win :-)");
    else
      Afpx.Encode_Field(Afpx_Xref.Game.Wins, (0, 33), "You win :-(");
    end if;
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Remove, True);
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Reset, True);
    Afpx.Encode_Field (Afpx_Xref.Game.Remove, (1, 1), "P l a y");
    Afpx.Set_Half_Col_Offset (Afpx_Xref.Game.Remove, 1, True);
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Play, True);
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      if Ptg_Result.Event = Afpx.Signal_Event
      or else (Ptg_Result.Event = Afpx.Keyboard
               and then Ptg_Result.Keyboard_Key = Afpx.Break_Key) then
        Logger.Log_Debug ("  Break");
        raise Common.Exit_Requested;
      end if;
      if Ptg_Result.Event = Afpx.Mouse_Button then
        if Ptg_Result.Field_No = Afpx_Xref.Game.Play then
          Logger.Log_Debug ("  Change game");
          Change_Game := True;
          exit;
        elsif Ptg_Result.Field_No = Afpx_Xref.Game.Remove then
          Logger.Log_Debug ("  Play");
          Change_Game := False;
          exit;
        elsif Ptg_Result.Field_No = Afpx_Xref.Game.Quit then
          Logger.Log_Debug ("  Exit");
          raise Common.Exit_Requested;
        elsif Ptg_Result.Field_No = Afpx_Xref.Game.Reset then
          Logger.Log_Debug ("  Reset");
          Common.Reset_Scores;
          Update_Scores;
          Afpx.Clear_Field(Afpx_Xref.Game.Wins);
        end if;
      end if;
    end loop;
    Afpx.Reset_Field (Afpx_Xref.Game.Remove);
    Afpx.Reset_Field (Afpx_Xref.Game.Wins);
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Play, False);
  end End_Game;

end Screen;

