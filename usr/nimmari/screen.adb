with Afpx, Con_Io, Normal;
use Afpx;

with Common;
package body Screen is

  -- All the bars
  subtype Index_Range is Positive range 1 .. 16;

  -- Status of one / all bars
  type Status_List is (Free, Selected, Removed);
  type Status_Tab is array (Index_Range) of Status_List;
  Status : Status_Tab;

  -- Stick color
  Stick_Color : constant Con_Io.Effective_Basic_Colors := Con_Io.Light_Gray;

  Human_Score : Natural;
  Machine_Score : Natural;

  function First_Index_Of_Row (Row : Common.Row_Range) return Index_Range is
    Index : Index_Range := Index_Range'First;
  begin
    for I in 2 .. Row loop
      Index := Index + Common.Bar_Per_Row(I - 1);
    end loop;
    return Index;
  end First_Index_Of_Row;

  function Last_Index_Of_Row (Row : Common.Row_Range) return Index_Range is
  begin
    return First_Index_Of_Row (Row) + Common.Bar_Per_Row (Row) - 1;
  end Last_Index_Of_Row;

  function Intro return Common.Game_List is
    -- For put then get
    Cursor_Field : Field_Range := Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Result : Result_Rec;
  begin
    Use_Descriptor (1);
    loop
      Put_Then_Get(Cursor_Field, Cursor_Col, Result, True);
      exit when Result.Event = Afpx.Mouse_Button;
      if Result.Event = Afpx.Signal_Event
      or else (Result.Event = Afpx.Keyboard
               and then Result.Keyboard_Key = Afpx.Break_Key) then
        raise Exit_Requested;
      end if;
    end loop;
    if Result.Field_No = 3 then
      return Common.Nim;
    elsif Result.Field_No = 4 then
      return Common.Marienbad;
    elsif Result.Field_No = 5 then
      raise Exit_Requested;
    end if;
    -- To avoid warning
    return Common.Nim;
  end Intro;

  procedure Reset (Game : in Common.Game_List) is
    use Common;
  begin
    Use_Descriptor (2);
    Score (Human_Score, Machine_Score);
    if Game = Common.Nim then
      Encode_Field (20, (0,0), "   Nim   ");
      Encode_Field (22, (1,1), "Play Marienbad");
    else
      Encode_Field (20, (0,0), "Marienbad");
      Encode_Field (22, (1,1), "   Play Nim");
    end if;
    Status := (others => Free);
    for I in Field_Range'(1) .. 16 loop
      Set_Field_Activation (I, True); 
      Set_Field_Colors (I, Background => Stick_Color);
    end loop;
    Set_Field_Activation (22, False);
  end Reset;

  procedure Play is
    -- For put then get
    Cursor_Field : Field_Range := Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Result : Result_Rec;
    Redisplay : Boolean;

    -- The current selection
    Selection_Index : Index_Range;
    Nb_Selected : Natural;
    Row_Selected : Common.Row_Range;

    -- Get the row of an index
    function Row_Of_Index (Index : Index_Range) return Common.Row_Range is
    begin
      case Index is
        when 01 .. 07 => return 1;
        when 08 .. 12 => return 2;
        when 13 .. 15 => return 3;
        when 16       => return 4;
      end case;
    end Row_Of_Index;

  begin
    Nb_Selected := 0;
    Row_Selected := 1;
    Redisplay := False;
    loop
      -- Activate play
      Set_Field_Activation (17, Nb_Selected /= 0);
      Put_Then_Get(Cursor_Field, Cursor_Col, Result, Redisplay);
      if Result.Event = Afpx.Signal_Event
      or else (Result.Event = Afpx.Keyboard
               and then Result.Keyboard_Key = Afpx.Break_Key) then
        raise Exit_Requested;
      end if;
      if Result.Event = Afpx.Mouse_Button then
        case Result.Field_No is
          when 1 .. 16 =>
            Selection_Index := Index_Range(Result.Field_No);
  
            -- First selection
            if Nb_Selected = 0 then
              Row_Selected := Row_Of_Index(Selection_Index);
            end if;

            -- Take selection / unselect
            if Row_Of_Index(Selection_Index) = Row_Selected then
              if Status(Selection_Index) = Selected then
                Status(Selection_Index)  := Free;
                Set_Field_Colors (Result.Field_No, Background => Stick_Color);
                Nb_Selected := Nb_Selected - 1;
              else
                Status(Selection_Index)  := Selected;
                Set_Field_Colors (Result.Field_No, Background => Con_Io.Red);
                Nb_Selected := Nb_Selected + 1;
              end if;
            end if;
          
          when 17 =>
            -- Take selection
            for I in First_Index_Of_Row(Row_Selected) .. Last_Index_Of_Row(Row_Selected) loop
              if Status(I) = Selected then
                Status(I) := Removed;
                Set_Field_Activation (Field_Range(I), False); 
              end if;
            end loop;
            exit;
          when 18 =>
            raise Exit_Requested;
          when others =>
            null; 
        end case;
        Redisplay := False;
      else
        Redisplay := True;
      end if;
    end loop;

  end Play;

  function Content (Row : Common.Row_Range) return Common.Full_Bar_Range is
    J : Natural := 0;
    Res : Common.Full_Bar_Range := 0;
  begin
    for I in reverse First_Index_Of_Row(Row) .. Last_Index_Of_Row(Row) loop
      if Status(I) = Free then
        Res := Res + 2 ** J;
      end if;
    end loop;
    return Res;
  end Content;


  procedure Update (Row : in Common.Row_Range; Bars : in Common.Full_Bar_Range;
                    Result : in Compute.Result_List; Change_Game : out Boolean) is
    Nb_To_Remove, J : Natural;
    -- For put then get
    Cursor_Field : Field_Range := Field_Range'First;
    Cursor_Col : Con_Io.Col_Range := Con_Io.Col_Range'First;
    Ptg_Result : Result_Rec;
    use Compute;
  begin
    Change_Game := False;
    -- Current content
    Nb_To_Remove := 0;
    for I in First_Index_Of_Row(Row) .. Last_Index_Of_Row(Row) loop
      if Status(I) = Free then
        Nb_To_Remove := Nb_To_Remove + 1;
      end if;
    end loop;
    -- Nb to remove
    Nb_To_Remove := Nb_To_Remove - Bars;

    -- Select
    J := Nb_To_Remove;
    for I in First_Index_Of_Row(Row) .. Last_Index_Of_Row(Row) loop
      if Status(I) = Free then
        Status(I)  := Selected;
        Set_Field_Colors (Field_Range(I), Background => Con_Io.Red);
        J := J - 1;
        exit when J = 0;
      end if;
    end loop;

    -- Display
    if Result in Compute.Played_Result_List then
      Set_Field_Activation (17, False);
      Put;
      delay 0.5;
    end if;

    -- Remove
    J := Nb_To_Remove;
    for I in First_Index_Of_Row(Row) .. Last_Index_Of_Row(Row) loop
      if Status(I) = Selected then
        Status(I) := Removed;
        Set_Field_Colors (Field_Range(I), Background => Con_Io.Red);
        Set_Field_Activation (Field_Range(I), False);
        J := J - 1;
        exit when J = 0;
      end if;
    end loop;

    -- Validate
    if Result /= Compute.Played then
      if Result = Compute.Played_And_Won or else Result = Compute.Won then
        Encode_Field(21, (0, 34), "I win :-)");
      else
        Encode_Field(21, (0, 33), "You win :-(");
      end if;
      Set_Field_Activation (17, True);
      Encode_Field (17, (1, 1), "P l a y");
      Set_Field_Activation (22, True);
      loop
        Put_Then_Get(Cursor_Field, Cursor_Col, Ptg_Result, True);
        if Ptg_Result.Event = Afpx.Mouse_Button and then Ptg_Result.Field_No = 22 then
          Change_Game := True;
        end if;
        exit when Ptg_Result.Event = Afpx.Mouse_Button
                  and then (Ptg_Result.Field_No = 17 or else Ptg_Result.Field_No = 22);
        if Ptg_Result.Event = Afpx.Mouse_Button and then Ptg_Result.Field_No = 18 then
          raise Exit_Requested;
        end if;
      end loop;
      Reset_Field (17);
      Reset_Field (21);
      Set_Field_Activation (22, False);
    end if;
  end Update;

  procedure Score (Human, Machine : in Natural) is
  begin
    Human_Score := Human;
    Machine_Score := Machine;
    Encode_Field (19, (0,  6), Normal(Human_Score, 3));
    Encode_Field (19, (0, 17), Normal(Machine_Score, 3));
  end Score;
  

end Screen;
