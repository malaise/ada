with Con_Io, Afpx, Timers, Basic_Proc;
with Afpx_Xref, Screen, Timer_Cb, Reg_Exp;
procedure Regex is

  -- Curent Ptg status
  Get_Handle   : Afpx.Get_Handle_Rec;

  -- Notified of new cursor field
  function Cursor_Set_Col (
      Cursor_Field : Afpx.Field_Range;
      New_Field : Boolean;
      Pointer_Col : Con_Io.Col_Range;
      Dummy_Offset : Con_Io.Col_Range;
      Dummy_Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
      Str : Afpx.Unicode_Sequence) return Con_Io.Col_Range is
  begin
    -- Update the current cursor col, move to last char and notify screen
    Get_Handle.Cursor_Field := Cursor_Field;
    Screen.Cursor_Has_Changed;
    if New_Field then
      return Afpx.Last_Index (Str, True);
    else
      return Pointer_Col;
    end if;
  end Cursor_Set_Col;

  -- Compile Regex, exec and set result
  procedure Update is
    Input : Screen.Input_Rec;
    Ok : Boolean;
    Line : Screen.Text_Range := Screen.Text_Range'First;
    Results : Screen.Results_Array;
    Pattern : Reg_Exp.Compiled_Pattern;
    N_Matched : Natural;
    Match_Info : Reg_Exp.Match_Array (1 .. Screen.Nb_Results);
    use type Afpx.Absolute_Field_Range;
  begin
    -- Get input
    Input := Screen.Get_Input;

    -- If Regex is empty then no output
    if Input.Regex.Is_Null then
      Screen.Put_Results (Results => Screen.No_Results);
      return;
    end if;

    -- Compile pattern
    Pattern.Compile (Ok, Input.Regex.Image, Input.Case_Sensitive);
    -- Handle compilation error
    if not Ok then
      Screen.Put_Error (Pattern.Error);
      return;
    end if;

    -- Check
    Ok := False;
    for I in Input.Text'Range loop
      if not Input.Text(I).Is_Null
      and then I >= Integer (Get_Handle.Cursor_Field - Screen.First_Text + 1)
      then
        Pattern.Exec (Input.Text(I).Image, N_Matched, Match_Info);
        if N_Matched /= 0 then
          -- Match found
          Line := I;
          Ok := True;
          exit;
        end if;
      end if;
    end loop;

    -- Set result
    if Ok then
      for I in 1 .. N_Matched loop
        if Reg_Exp.Valid_Match (Match_Info(I)) then
          Results(I) := (
              Start => Match_Info(I).First_Offset,
              Stop => Match_Info(I).Last_Offset_Stop,
              Str => Input.Text(Line).Uslice (
                       Match_Info(I).First_Offset,
                       Match_Info(I).Last_Offset_Stop) );
        end if;
      end loop;
    end if;

    -- Put result
    Screen.Put_Results (Get_Handle.Cursor_Field, Line, Results);

  end Update;

  -- Timer info
  Unused_Tid : Timers.Timer_Id;
  Period : constant Duration := 0.3;

  -- Afpx Ptg data
  Ptg_Result   : Afpx.Result_Rec;

begin
  -- Init Afpx
  Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
  -- Start timers
  Unused_Tid := Timers.Create (
      (Delay_Kind => Timers.Delay_Sec,
       Clock => null,
       Period => Period,
       Delay_Seconds => Period),
      Timer_Cb'Access);

  -- Main loop
  loop
    Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
                       Cursor_Col_Cb => Cursor_Set_Col'Unrestricted_Access);
    case Ptg_Result.Event is
      when Afpx.Mouse_Button =>
        -- Handle buttons
        case Ptg_Result.Field_No is
          -- Toggle the option
          when Afpx_Xref.Main.Case_Sensitive =>
            Screen.Toggle (Ptg_Result.Field_No);
          when Afpx_Xref.Main.Clear_Regex =>
            Screen.Clear_Regex;
            Get_Handle.Cursor_Field := Afpx_Xref.Main.Regex;
            Get_Handle.Cursor_Col := Con_Io.Col_Range'First;
          when Afpx_Xref.Main.Clear_Text =>
            Screen.Clear_Text;
            Get_Handle.Cursor_Field := Afpx_Xref.Main.Text1;
            Get_Handle.Cursor_Col := Con_Io.Col_Range'First;
          when Afpx_Xref.Main.Quit =>
            return;
          when others =>
            null;
        end case;
        when Afpx.Keyboard =>
          -- Handle key
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              Update;
            when Afpx.Escape_Key =>
              null;
            when Afpx.Break_Key =>
              -- Quit
              return;
        end case;
      when Afpx.Timer_Event =>
        -- Timer Cb has detected that intput has changed
        Update;
      when Afpx.Refresh =>
        -- Check if intput has changed
        if Screen.Input_Changed then
          Update;
        end if;
      when others =>
        -- All other events
        null;
    end case;

  end loop;

exception
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Regex;

