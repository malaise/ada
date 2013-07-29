with Afpx, Con_Io, Normal, Str_Util, Afpx_Xref;
separate (Renardeau)

package body X is
  use Afpx;

  -- Fields
  -- First of Base
  Base_Fs : constant Field_Range := Afpx_Xref.Main.Base_Fs;
  -- Target
  Target_F : constant Field_Range := Afpx_Xref.Main.Target_F;
  -- First of numbers
  Number_Fs : constant Field_Range := Afpx_Xref.Main.Number_Fs;
  -- First of digits
  Digit_Fs : constant Field_Range := Afpx_Xref.Main.Digit_Fs;
  -- Zero
  Zero_F : constant Field_Range := Afpx_Xref.Main.Zero_F;
  -- Enter
  Enter_F : constant Field_Range := Afpx_Xref.Main.Enter_F;
  -- Undo
  Undo_F : constant Field_Range := Afpx_Xref.Main.Undo_F;
  -- Reset
  Clear_F : constant Field_Range := Afpx_Xref.Main.Clear_F;
  -- Result
  Result_F : constant Field_Range := Afpx_Xref.Main.Result_F;
  -- First of computations
  Compute_Fs : constant Field_Range := Afpx_Xref.Main.Compute_Fs;
  -- Exit
  Exit_F : constant Field_Range := Afpx_Xref.Main.Exit_F;

  -- For PTG Get field (there is none here)
  Cursor_Field : Field_Range := 1;
  Cursor_Col   : Con_Io.Col_Range := 0;
  Insert       : Boolean := False;
  Redisplay    : Boolean := False;

  Ptg_Result   : Result_Rec;

  Deactivated : constant Con_Io.Colors := Con_Io.Color_Of ("Red");
  Activated : Con_Io.Colors;

  -- Status while getting
  type Status_List is (B1, B2, B3, B4, B5, B6, T1, T2, T3, Ready, Done);

  -- Memory of previous status
  Number_Act, Digit_Act, Zero_Act, Enter_Act, Clear_Act : Boolean := False;
  Prev_Status : Status_List := Done;

  -- Put field associated to status
  subtype Put_List is Status_List range B1 .. T3;
  function Field_Of (Put : Put_List) return Field_Range is
  begin
    case Put is
      when B1 => return Base_Fs + 0;
      when B2 => return Base_Fs + 1;
      when B3 => return Base_Fs + 2;
      when B4 => return Base_Fs + 3;
      when B5 => return Base_Fs + 4;
      when B6 => return Base_Fs + 5;
      when T1 .. T3 => return Target_F;
    end case;
  end Field_Of;

  procedure Activate_Fields (Status : in Status_List; Force : in Boolean) is
    -- Field active
    Active : Boolean;
  begin
    -- Reset / Set backround color of input field
    if Status in B1 .. Ready
    and then (Prev_Status /= Status or else Force) then
      -- Reset backround of Prev_Status
      if Prev_Status in Put_List then
        Reset_Field (Field_Of (Prev_Status), Reset_String => False);
      end if;
      -- Set Background of new Status
      if Status in Put_List then
        Set_Field_Colors (Field_Of (Status),
                          Background => Con_Io.Color_Of ("Lime_Green"));
      end if;
      Prev_Status := Status;
    end if;

    -- Activate buttons depending on status
    -- Numbers: only when getting bases
    Active := Status in B1 .. B6;
    if Number_Act /= Active or else Force then
      for I in Number_Fs .. Number_Fs + 4 loop
        Set_Field_Protection (I, not Active);
        if Active then
          Set_Field_Colors (I, Activated);
        else
          Set_Field_Colors (I, Deactivated);
        end if;
      end loop;
      Number_Act := Active;
    end if;

    -- Digits: when getting bases and target
    Active := Status in B1 .. T3;
    if Digit_Act /= Active or else Force then
      for I in Digit_Fs .. Digit_Fs + 8 loop
        Set_Field_Protection (I, not Active);
        if Active then
          Set_Field_Colors (I, Activated);
        else
          Set_Field_Colors (I, Deactivated);
        end if;
      end loop;
      Digit_Act := Active;
    end if;

    -- Zero: when getting target tenth and unit
    Active := Status in T2 .. T3;
    if Zero_Act /= Active or else Force then
      Set_Field_Protection (Zero_F, not Active);
      if Active then
        Set_Field_Colors (Zero_F, Activated);
       else
        Set_Field_Colors (Zero_F, Deactivated);
      end if;
      Zero_Act := Active;
    end if;

    -- Enter: when got target
    Active := Status = Ready or else Status = Done;
    if Enter_Act /= Active or else Force then
      Set_Field_Protection (Enter_F, not Active);
      if Active then
        Set_Field_Colors (Enter_F, Activated);
       else
        Set_Field_Colors (Enter_F, Deactivated);
      end if;
      Enter_Act := Active;
    end if;

    -- Clear: when getting bases and target
    Active := Status in B2 .. Ready;
    if Clear_Act /= Active or else Force then
      Set_Field_Protection (Undo_F, not Active);
      if Active then
        Set_Field_Colors (Undo_F, Activated);
      else
        Set_Field_Colors (Undo_F, Deactivated);
      end if;
      Clear_Act := Active;
    end if;

  end Activate_Fields;

  procedure Get_Inputs (Bases : out Bases_Array;
                        Target : out Positive;
                        Cancel : out Boolean) is
    -- Get a value from a field at a row
    function Get_Value (Field : in Field_Range; Row : in Con_Io.Row_Range)
             return Natural is
    begin
      return Natural'Value (Str_Util.Strip (Decode_Field (Field, Row),
                                              Str_Util.Both));
    end Get_Value;

    procedure Put_Base (No : Natural) is
      Val : constant Natural := Bases(Bases_Range(No));
    begin
      if Val < 10 then
        Encode_Field (Base_Fs + Field_Range(No) - 1, (0, 1), Normal (Val, 1));
      else
        Encode_Field (Base_Fs + Field_Range(No) - 1, (0, 0), Normal (Val, 3));
      end if;
    end Put_Base;

    -- Status
    Status : Status_List;
    -- A value
    Value : Natural;
    -- Offset in target, base...
    Offset : Natural;
    -- Force reset of activation
    Reset : Boolean;
  begin
    -- Init / Reset
    Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    Reset := True;

    -- Get foreground color
    declare
      Background : Con_Io.Effective_Colors;
      Selected   : Con_Io.Effective_Colors;
    begin
      Get_Field_Colors (Exit_F, Activated, Background, Selected);
    end;

    -- Init for loop
    Status := B1;
    Prev_Status := Done;
    loop
      -- Activate the fields according to Status
      Activate_Fields (Status, Reset);
      Reset := False;

      -- Ptg
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Redisplay,
                         Ptg_Result);
      case Ptg_Result.Event is
        when Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Return_Key | Escape_Key =>
              null;
            when Afpx.Break_Key =>
              Cancel := True;
              return;
          end case;
        when Mouse_Button =>
          case Ptg_Result.Field_No is
            when Number_Fs .. Number_Fs + 4 =>
              Offset := Status_List'Pos(Status) - Status_List'Pos(B1) + 1;
              Bases(Bases_Range(Offset)) := Get_Value (Ptg_Result.Field_No, 1);
              Put_Base (Offset);
              Status := Status_List'Succ (Status);
            when Digit_Fs .. Digit_Fs + 8 | Zero_F =>
              if Status in B1 .. B6 then
                Offset := (Status_List'Pos(Status) - Status_List'Pos(B1) + 1);
                Bases(Bases_Range(Offset)) :=
                          Get_Value (Ptg_Result.Field_No, 1);
                Put_Base (Offset);
              else
                -- Target: Add new digit
                Offset := Status_List'Pos(Status) - Status_List'Pos(T1);
                Value := Get_Value (Ptg_Result.Field_No, 1);
                Encode_Field (Target_F, (0, Offset), Normal (Value, 1));
              end if;
              Status := Status_List'Succ (Status);
              if Status = Ready then
                Target := Get_Value (Target_F, 0);
              end if;
            when Undo_F =>
              if Status in B2 .. T1 then
                Offset := Bases_Range (Status_List'Pos(Status)
                                     - Status_List'Pos(B1));
                Clear_Field (Base_Fs + Field_Range(Offset) - 1);
              elsif Status in T2 .. Ready then
                Clear_Field (Target_F);
                Status := T1;
              end if;
            when Clear_F =>
              Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
              Reset := True;
              Status := B1;
              Prev_Status := Ready;
            when Enter_F =>
              Cancel := False;
              return;
            when Exit_F =>
              Cancel := True;
              return;
            when others =>
              null;
          end case;
        when Fd_Event | Timer_Event | Signal_Event =>
          null;
        when Refresh =>
          Redisplay := True;
      end case;

    end loop;
  end Get_Inputs;

  procedure Put_Outputs (Found : Boolean;
                         Outputs : in Unbounded_Ouputs.Unbounded_Array;
                         Finish : out Boolean) is
    -- Put a value (on 3 digits) in a field at a row
    procedure Encode (Field : in Field_Range;
                      Col : in Con_Io.Col_Range;
                      Value : in Natural) is
    begin
      Encode_Field (Field, (0, Col), Normal (Value, 4));
    end Encode;
    Field : Field_Range;
    Output : Output_Rec;
  begin
    -- Allow Reset and Exit
    Set_Field_Protection (Enter_F, True);
    Set_Field_Colors (Enter_F, Deactivated);
    Set_Field_Protection (Undo_F, True);
    Set_Field_Colors (Undo_F, Deactivated);
    -- Set color of Result to Blue if not found, put result
    if not Found then
      Set_Field_Colors (Result_F, Con_Io.Color_Of ("Blue"));
    end if;
    Encode (Result_F, 0, Outputs.Element(Outputs.Length).Result);

    -- Put computation
    if Is_No_Operation (Outputs.Element(1)) then
      Encode (Compute_Fs, 14, Outputs.Element(1).Result);
    else
      for I in 1 .. Outputs.Length loop
        Field := Compute_Fs + Absolute_Field_Range(I - 1);
        Output := Outputs.Element(I);
        Encode (Field, 0, Output.Left);
        Encode_Field (Field, (0, 5), Operations_Images (Output.Operation) & "");
        Encode (Field, 7, Output.Right);
        Encode_Field (Field, (0, 12), "=");
        Encode (Field, 14, Output.Result);
      end loop;
    end if;

    Insert := False;
    Redisplay := False;
    loop
      -- Ptg
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Redisplay,
                         Ptg_Result);
      case Ptg_Result.Event is
        when Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Return_Key | Escape_Key =>
              null;
            when Afpx.Break_Key =>
              Finish := True;
              return;
          end case;
        when Mouse_Button =>
          case Ptg_Result.Field_No is
            when Clear_F =>
              Finish := False;
              return;
            when Exit_F =>
              Finish := True;
              return;
            when others =>
              null;
          end case;
        when Fd_Event | Timer_Event | Signal_Event =>
          null;
        when Refresh =>
          Redisplay := True;
      end case;

    end loop;
  end Put_Outputs;
end X;

