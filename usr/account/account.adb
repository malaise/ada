-- Usage: account [ -e | -f ] [ <file> ]
--  (default is euros)
with Ada.Exceptions;
with Basic_Proc, Argument, Con_Io, Afpx;
with Unit_Format, Screen, Mng;
procedure Account is

  -- Afpx put_then_get stuff
  Cursor_Field : Afpx.Absolute_Field_Range := 1;
  Cursor_Col   : Con_Io.Col_Range := 0;
  Insert       : Boolean := False;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean := False;


  -- Parsing
  Usage_Error : exception;

  procedure Set_Unit (Str : in String) is
  begin
    if Str = "-f" then
      Unit_Format.Set_Unit_To(Unit_Format.Francs);
    elsif Str = "-e" then
      Unit_Format.Set_Unit_To(Unit_Format.Euros);
    else
      raise Usage_Error;
    end if;
  end Set_Unit;

  -- We have to quit program: raises Quit_Program
  Quit_Program : exception;
  procedure Quit is separate;

begin

  -- Check arguments
  -- Screen.reset must be called before mng.load(which uses the screen),
  --  but after sanity checks (which don't need the screen to be loaded)
  declare
    File_Arg : Natural := 0;
  begin
    if Argument.Get_Nbre_Arg = 0 then
      null;
    elsif Argument.Get_Nbre_Arg = 1 then
      if Argument.Get_Parameter = "-h" then
        raise Usage_Error;
      end if;
      begin
        -- Try to set unit
        Set_Unit(Argument.Get_Parameter);
      exception
        when Usage_Error =>
          -- Not valid unit mode -> file
          File_Arg := 1;
      end;
    elsif Argument.Get_Nbre_Arg = 2 then
      Set_Unit(Argument.Get_Parameter(1));
      File_Arg := 2;
    else
      raise Usage_Error;
    end if;

    -- Init the screen
    Screen.Reset;
    Screen.Set_Sublist(False);
    Screen.Allow_Edit(False);

    -- No data
    Mng.Clear;

    -- Load file if any
    if File_Arg /= 0 then
      Mng.Load(Argument.Get_Parameter(File_Arg));
    end if;
  end;

  -- Now the main loop
  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                       Ptg_Result, Redisplay);
    if Afpx.Line_List.Is_Empty then
      Mng.Set_Current (0);
    else
      Mng.Set_Current (Afpx.Line_List.Get_Position);
    end if;
    Redisplay := False;
    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            null;
          when Afpx.Escape_Key =>
            Quit;
          when Afpx.Break_Key =>
            Quit;
        end case;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is
          -- Double click in list
          when Afpx.List_Field_No =>
            Mng.Update_State;

          -- List movements
          when Screen.List_Top_Fld =>
            -- Top
            Afpx.Update_List(Afpx.Top);
          when Screen.List_Pgup_Fld =>
            -- PgUp
            Afpx.Update_List(Afpx.Page_Up);
          when Screen.List_Up_Fld =>
            -- Up
            Afpx.Update_List(Afpx.Up);
          when Screen.List_Center_Fld =>
            -- Center current
            Afpx.Update_List(Afpx.Center_Selected);
          when Screen.List_Down_Fld =>
            -- Down
            Afpx.Update_List(Afpx.Down);
          when Screen.List_Pg_Down_Fld =>
            -- PgDown
            Afpx.Update_List(Afpx.Page_Down);
          when Screen.List_Bottom_Fld =>
            -- Bottom
            Afpx.Update_List(Afpx.Bottom);

          -- Oper actions
          when Screen.Add_Oper_Fld =>
            -- Add
            Mng.Add_Oper;
          when Screen.Copy_Oper_Fld =>
            -- Copy
            Mng.Copy_Oper;
          when Screen.Edit_Oper_Fld =>
            -- Edit
            Mng.Edit_Oper;
          when Screen.Delete_Oper_Fld =>
            -- Delete
            Mng.Del_Oper;
          when Screen.Clean_Oper_Fld =>
            -- Update
            Mng.Garbage_Collect;
          when Screen.Search_Oper_Fld =>
            -- Search
            Mng.Search;
          when Screen.Show_Oper_Fld =>
            -- Show all
            Mng.Show_All;

          -- Account actions
          when Screen.New_Account_Fld =>
            -- Create
            Mng.Clear;
          when Screen.Load_Account_Fld =>
            -- Load
            Mng.Load("");
          when Screen.Save_Account_Fld =>
            -- Save
            Mng.Save(Mng.Select_New);
          when Screen.Print_Account_Fld =>
            -- Print
            Mng.Print;
          when Screen.Franc_Account_Fld =>
            -- Switch francs/euros
            Mng.Change_Unit;
          when Screen.Sdat_Account_Fld =>
            -- Sort by date
            Mng.Sort (By_Date => True);
          when Screen.Samo_Account_Fld =>
            -- Sort by abs(amount)
            Mng.Sort (By_Date => False);
          when Screen.Exit_Account_Fld =>
            -- Exit
            Quit;
          when others =>
            Screen.Ack_Error(Screen.Internal_Error);
            Mng.Save(Mng.Rescue);
        end case;
      when Afpx.Refresh =>
        Redisplay := True;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
        null;
    end case;
  end loop;

exception
  when Usage_Error =>
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                    & "[ -e | -f ] [ <account_file> ]");
    Basic_Proc.Put_Line_Output ("  (default is euros)");
  when Quit_Program =>
    Afpx.Release_Descriptor;
  when Ooops : others =>
    begin
      Screen.Reset;
      Screen.Ack_Error(Screen.Internal_Error);
    exception
      when others => null;
    end;
    begin
      Basic_Proc.Put_Line_Error ("Exception: "
             & Ada.Exceptions.Exception_Name(Ooops));
    exception
      when others => null;
    end;
    Mng.Save(Mng.Rescue);
end Account;

