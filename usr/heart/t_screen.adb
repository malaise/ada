with Calendar;
with Argument, Afpx, Con_Io, Dynamic_List, Dir_Mng, Normal, Dos;
procedure T_Screen is

  Afpx_Item : Afpx.Line_Rec;

  Dscr : Afpx.Descriptor_Range;

  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay : Boolean;


  Curr_Date  : Afpx.Absolute_Field_Range;

  Exit_Button : Afpx.Absolute_Field_Range;
  First_Get   : Afpx.Absolute_Field_Range;


  List_Empty : Boolean;

  Allow_Undo  : Boolean;
  Allow_Draw  : Boolean;
  In_Add : Boolean;

  In_Edit  : Boolean;
  In_Valid : Boolean;


  Act : Boolean;

  use Afpx;
begin
  if Argument.Get_Nbre_Arg < 1 then
    return;
  end if;

  Dscr := Afpx.Descriptor_Range'Value (Argument.Get_Parameter);

  declare
    procedure Dir_Sort is new Dir_Mng.File_List_Mng.Sort (Dir_Mng.Less_Than);
    Dir_List : Dir_Mng.File_List_Mng.List_Type;
    Dir_Item : Dir_Mng.File_Entry_Rec;
  begin
    -- List directory and store it in Afpx list
    if Argument.Get_Nbre_Arg = 1 then
      Dir_Mng.List_Dir (Dir_List, "");
    else
      Dir_Mng.List_Dir (Dir_List,
                        Argument.Get_Parameter (Occurence => 2));
    end if;

    -- Sort, move to first, copy in Afpx list
    Dir_Sort (Dir_List);
    Dir_Mng.File_List_Mng.Move_To (Dir_List, Dir_Mng.File_List_Mng.Next,
                                  0 , False);
    loop
      Dir_Mng.File_List_Mng.Read (Dir_List, Dir_Item,
                                  Dir_Mng.File_List_Mng.Current);
      Afpx_Item.Len := Dir_Item.Len;
      Afpx_Item.Str := (others => ' ');
      Afpx_Item.Str(1 .. Afpx_Item.Len) := Dir_Item.Name (1 .. Dir_Item.Len);
      Afpx.Line_List_Mng.Insert (Afpx.Line_List, Afpx_Item);
      exit when Dir_Mng.File_List_Mng.Get_Position (Dir_List)
              = Dir_Mng.File_List_Mng.List_Length (Dir_List);
      Dir_Mng.File_List_Mng.Move_To (Dir_List);
    end loop;
    -- End of list
    Afpx.Line_List_Mng.Move_To (Afpx.Line_List, Afpx.Line_List_Mng.Prev,
                                0, False);
  exception
    when Dir_Mng.Name_Error =>
      null;
  end;

  Afpx.Use_Descriptor(Dscr);

  if Dscr = 1 then
    Exit_Button  := 19;
    First_Get := 07;
    Allow_Undo := False;
    Curr_Date := 02;
  elsif Dscr = 2 then
    Exit_Button  := 5;
    First_Get := 11;
    In_Edit := False;
    In_Valid := False;
    Curr_Date := 02;
  elsif Dscr = 3 then
    Exit_Button := 125;
    First_Get := 04;
    In_Edit := True;
    Curr_Date := 02;
  end if;

  Cursor_Field := First_Get;
  Cursor_Col := 0;
  Redisplay := False;


  -- Encode date
  declare
    Current_Time : constant Calendar.Time := Calendar.Clock;
  begin
    Afpx.Encode_Field (Curr_Date, (0, 0),
      Normal(Calendar.Day(Current_Time)  , 2, Gap => '0') & "/"
    & Normal(Calendar.Month(Current_Time), 2, Gap => '0') & "/"
    & Normal(Calendar.Year(Current_Time),  4, Gap => '0') );
  end;


  loop

    List_Empty := Afpx.Line_List_Mng.List_Length(Afpx.Line_List) = 0;
    if Dscr = 1 then
      Allow_Draw := not List_Empty and then
                    Afpx.Line_List_Mng.List_Length(Afpx.Line_List) <= 10;
      Afpx.Set_Field_Activation (22, not List_Empty);
      Afpx.Set_Field_Activation (23, Allow_Draw);
      Afpx.Set_Field_Activation (24, not List_Empty);
      Afpx.Set_Field_Activation (26, not List_Empty);
      Afpx.Set_Field_Activation (27, not List_Empty);
      Afpx.Set_Field_Activation (17, Allow_Undo);
      Afpx.Encode_Field (20, (0, 0),
        Normal(Afpx.Line_List_Mng.List_Length(Afpx.Line_List), 5) );
    elsif Dscr = 2 then
      -- List and menu buttons, not in valid nor edit
      -- Same for create
      Act := not In_Valid and then not In_Edit;
      Afpx.Set_Field_Activation (00, Act);
      Afpx.Set_Field_Activation (03, Act);
      Afpx.Set_Field_Activation (04, Act);
      Afpx.Set_Field_Activation (05, Act);
      Afpx.Set_Field_Activation (06, Act);
      -- Delete/edit if not empty and not in valid nor edit
      Act := Act and then not List_Empty;
      Afpx.Set_Field_Activation (07, Act);
      Afpx.Set_Field_Activation (08, Act);
      -- Edit if edit
      Act := In_Edit or else In_Valid;
      for I in Afpx.Field_Range'(10) .. 23 loop
        Afpx.Set_Field_Activation (I, Act);
      end loop;
      -- Un protect get in edit
      Act := not In_Edit;
      Afpx.Set_Field_Protection (11, Act);
      Afpx.Set_Field_Protection (13, Act);
      Afpx.Set_Field_Protection (16, Act);
      Afpx.Set_Field_Protection (17, Act);
      Afpx.Set_Field_Protection (18, Act);
      Afpx.Set_Field_Protection (19, Act);
      Afpx.Set_Field_Protection (20, Act);
      Afpx.Set_Field_Protection (21, Act);
      Afpx.Set_Field_Activation (24, not Act);
      if Act then
        Afpx.Set_Field_Colors (11, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (13, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (16, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (17, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (18, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (19, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (20, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
        Afpx.Set_Field_Colors (21, Foreground => Con_Io.Cyan,
                                   Background => Con_Io.Black);
      else
        Afpx.Set_Field_Colors (11, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (13, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (16, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (17, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (18, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (19, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (20, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
        Afpx.Set_Field_Colors (21, Foreground => Con_Io.Brown,
                                   Background => Con_Io.Blue);
      end if;
      -- Compute if in edit
      Afpx.Set_Field_Activation (24, In_Edit);
      -- Confirm if Valid
      Afpx.Set_Field_Activation (09, In_Valid);
    elsif Dscr = 3 then
      null;
    end if;


    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result);
    Redisplay := False;

    case Ptg_Result.Event is

      when Refresh =>
        Redisplay := True;

      when Keyboard =>

        case Ptg_Result.Keyboard_Key is
          when Return_Key | Break_Key =>
            null;
          when Escape_Key =>
            -- Clear current field
            Afpx.Clear_Field (Cursor_Field);
            Cursor_Col := 0;
            Allow_Undo := False;
        end case;

      when Mouse_Button =>

        exit when Ptg_Result.Field_No = Exit_Button;
        if Dscr = 1 then
          case Ptg_Result.Field_No is
            when 15 | 16 =>
              -- Add, rem select : confirm
              for I in Afpx.Absolute_Field_Range range 0 .. 27 loop
                Afpx.Set_Field_Activation (I, False);
              end loop;
              Afpx.Set_Field_Activation (15, True);
              Afpx.Set_Field_Activation (16, True);
              Afpx.Set_Field_Activation (20, True);
              if Ptg_Result.Field_No = 15 then
                In_Add := True;
                Afpx.Clear_Field(16);
                Afpx.Encode_Field(16, (01, 04), "Abort");
                Afpx.Encode_Field(20, (00, 00),
                   "Add xxxxx records to the selection");
              else
                In_Add := False;
                Afpx.Clear_Field(15);
                Afpx.Encode_Field(15, (01, 04), "Abort");
                Afpx.Encode_Field(20, (00, 00),
                   "Remove xxxxx records from the selection");
              end if;
              Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result);
              -- Here we can confirm or abort
              if      (    In_Add and then Ptg_Result.Field_No = 15)
              or else (not In_Add and then Ptg_Result.Field_No = 16) then
                Allow_Undo := True;
              else
                Allow_Undo := False;
              end if;

              for I in Afpx.Absolute_Field_Range range 0 .. 27 loop
                Afpx.Set_Field_Activation (I, True);
              end loop;
              Afpx.Set_Field_Activation (20, False);
              Afpx.Reset_Field (15);
              Afpx.Reset_Field (16);
            when  22 | 27 =>
              -- Unselect, delete
              if Afpx.Line_List_Mng.Get_Position (Afpx.Line_List) /=
                 Afpx.Line_List_Mng.List_Length (Afpx.Line_List) then
                Afpx.Line_List_Mng.Delete (Afpx.Line_List);
              else
                Afpx.Line_List_Mng.Delete (Afpx.Line_List,
                                           Afpx.Line_List_Mng.Prev);
              end if;
            when others =>
              Allow_Undo := False;
          end case;
        elsif Dscr = 2 then
          case Ptg_Result.Field_No is
            when 06 | 08 =>
              -- Create, edit
              In_Edit := True;
              In_Valid := False;
              Cursor_Field := First_Get;
            when 07 =>
              -- Delete
              In_Edit := False;
              In_Valid := True;
            when 22 | 23 =>
              -- Valid, cancel
              In_Edit := False;
              In_Valid := False;
            when others =>
              null;
          end case;
        elsif Dscr = 3 then
          null;
        end if;

      when others =>
        null;
    end case;

  end loop;

  Con_Io.Reset_Term;

exception
  when others =>
    Con_Io.Bell (3);
    delay 5.0;
    Con_Io.Reset_Term;
    raise;
end T_Screen;
