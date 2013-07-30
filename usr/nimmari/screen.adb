with Afpx, Con_Io, Normal, Afpx_Xref;
package body Screen is

  -- Stick color
  Stick_Color : constant Con_Io.Effective_Colors
              := Con_Io.Color_Of ("Light_Grey");
  Sel_Color : constant Con_Io.Effective_Colors
            := Con_Io.Color_Of ("Red");

  function Intro return Common.Game_Kind_List is
    -- For put then get
    Cursor_Field : Afpx.Field_Range := Afpx.Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Insert : Boolean := False;
    Result : Afpx.Result_Rec;
    use type Afpx.Event_List, Afpx.Keyboard_Key_List, Afpx.Absolute_Field_Range;
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Intro.Dscr_Num);
    loop
      Afpx.Put_Then_Get(Cursor_Field, Cursor_Col, Insert, Result);
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

  procedure Reset is
    Scores : constant Common.Score_Array := Common.Get_Scores;
    use Afpx;
    use type Common.Game_Kind_List;
  begin
    if not Is_Descriptor_Set
    or else Get_Descriptor /= Afpx_Xref.Game.Dscr_Num then
      Use_Descriptor (Afpx_Xref.Game.Dscr_Num);
    end if;
    Encode_Field (Afpx_Xref.Game.Names, (0,  1),
                  "You: " & Normal (Scores(Common.Human), 3));
    Encode_Field (Afpx_Xref.Game.Names, (0, 13),
                  "Me: " & Normal (Scores(Common.Machine), 3));
    Clear_Field (Afpx_Xref.Game.Game);
    Clear_Field (Afpx_Xref.Game.Play);
    if Common.Get_Game_Kind = Common.Nim then
      Encode_Field (Afpx_Xref.Game.Game, (0,0), "   Nim");
      Encode_Field (Afpx_Xref.Game.Play, (1,1), "Play Marienbad");
    else
      Encode_Field (Afpx_Xref.Game.Game, (0,0), "Marienbad");
      Encode_Field (Afpx_Xref.Game.Play, (1,1), "   Play Nim");
    end if;
    Set_Field_Activation (Afpx_Xref.Game.Play, False);
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
    Cursor_Field : Afpx.Field_Range := Afpx.Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Insert : Boolean := False;
    Result : Afpx.Result_Rec;

    -- The current selection
    Selection_Index : Common.Index_Range;
    Nb_Selected : Natural;
    Row_Selected : Common.Row_Range;
    Row_Col : Common.Row_Col_Rec;
    type Selected_Array is array (Common.Index_Range) of Boolean;
    Selected : Selected_Array := (others => False);


    use type Afpx.Event_List, Afpx.Keyboard_Key_List;
  begin
    -- Init
    Init;
    for I in Common.Index_Range'Range loop
      Selected(I) := False;
    end loop;

    Nb_Selected := 0;
    Row_Selected := 1;
    loop
      -- Activate play
      Afpx.Set_Field_Activation (Afpx_Xref.Game.Remove, Nb_Selected /= 0);
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Result);
      if Result.Event = Afpx.Signal_Event
      or else (Result.Event = Afpx.Keyboard
               and then Result.Keyboard_Key = Afpx.Break_Key) then
        raise Common.Exit_Requested;
      end if;
      if Result.Event = Afpx.Mouse_Button then
        case Result.Field_No is
          when Common.Index_Range =>
            Selection_Index := Result.Field_No;
            Row_Col := Common.Index2Row_Col(Selection_Index);

            -- First selection
            if Nb_Selected = 0 then
              Row_Selected := Row_Col.Row;
            end if;

            -- Take selection / unselect
            if Row_Col.Row = Row_Selected then
              if Selected(Selection_Index) then
                Selected(Selection_Index) := False;
                Afpx.Set_Field_Colors (Result.Field_No,
                                       Background => Stick_Color);
                Nb_Selected := Nb_Selected - 1;
              else
                Selected(Selection_Index) := True;
                Afpx.Set_Field_Colors (Result.Field_No,
                                       Background => Sel_Color);
                Nb_Selected := Nb_Selected + 1;
              end if;
            end if;

          when Afpx_Xref.Game.Remove =>
            -- Take selection: set Row and Bars to remove
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
    Cursor_Field : Afpx.Field_Range := Afpx.Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Insert : Boolean := False;
    Ptg_Result : Afpx.Result_Rec;
    use type Common.Played_Result_List;
    use type Afpx.Event_List, Afpx.Absolute_Field_Range;
  begin
    for I in Common.Index_Range'Range loop
      Afpx.Set_Field_Protection(I, True);
    end loop;
    -- Validate
    if Result = Common.Played_And_Won or else Result = Common.Won then
      Afpx.Encode_Field(Afpx_Xref.Game.Wins, (0, 34), "I win :-)");
    else
      Afpx.Encode_Field(Afpx_Xref.Game.Wins, (0, 33), "You win :-(");
    end if;
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Remove, True);
    Afpx.Encode_Field (Afpx_Xref.Game.Remove, (1, 1), "P l a y");
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Play, True);
    loop
      Afpx.Put_Then_Get(Cursor_Field, Cursor_Col, Insert, Ptg_Result);
      if Ptg_Result.Event = Afpx.Mouse_Button then
        if Ptg_Result.Field_No = Afpx_Xref.Game.Play then
          Change_Game := True;
          exit;
        elsif Ptg_Result.Field_No = Afpx_Xref.Game.Remove then
          Change_Game := False;
          exit;
        elsif Ptg_Result.Field_No = 18 then
          raise Common.Exit_Requested;
        end if;
      end if;
    end loop;
    Afpx.Reset_Field (Afpx_Xref.Game.Remove);
    Afpx.Reset_Field (Afpx_Xref.Game.Wins);
    Afpx.Set_Field_Activation (Afpx_Xref.Game.Play, False);
  end End_Game;

end Screen;

