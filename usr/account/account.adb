-- Usage: account [ -e | -f ] [ <file> ]
--  (default is euros) 
with Ada.Text_Io, Ada.Exceptions;
with Argument, Con_Io, Afpx;
with Unit_Format, Oper_Def, Screen, Mng;
procedure Account is

  -- Afpx put_then_get stuff
  Cursor_Field : Afpx.Absolute_Field_Range := 1;
  Cursor_Col   : Con_Io.Col_Range := 0;
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
  --  but after sanity checks (which don't the screen to be loaded)
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

    -- No data
    Mng.Clear;

    -- Load file if any
    if File_Arg /= 0 then
      Mng.Load(Argument.Get_Parameter(File_Arg));
    end if;
  end;

  -- Now the main loop
  loop
    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
    Mng.Set_Current (Ptg_Result.Id_Selected);
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
          when 17 =>
            -- Top
            Afpx.Update_List(Afpx.Top);
          when 18 =>
            -- PgUp
            Afpx.Update_List(Afpx.Page_Up);
          when 19 =>
            -- Up
            Afpx.Update_List(Afpx.Up);
          when 20 =>
            -- Center
            Afpx.Update_List(Afpx.Center);
          when 21 =>
            -- Down
            Afpx.Update_List(Afpx.Down);
          when 22 =>
            -- PgDown
            Afpx.Update_List(Afpx.Page_Down);
          when 23 =>
            -- Bottom
            Afpx.Update_List(Afpx.Bottom);

          -- Oper actions
          when 25 =>
            -- Add
            Mng.Add_Oper;
          when 26 =>
            -- Copy
            Mng.Copy_Oper;
          when 27 =>
            -- Edit
            Mng.Edit_Oper;
          when 28 =>
            -- Delete
            Mng.Del_Oper;
          when 29 =>
            -- Update
            Mng.Garbage_Collect;
          when 30 =>
            -- Search
            Mng.Search;
          when 31 =>
            -- Show all
            Mng.Show_All;

          -- Account actions
          when 33 =>
            -- Create
            Mng.Clear;
          when 34 =>
            -- Load
            Mng.Load("");
          when 35 =>
            -- Save
            Mng.Save(Mng.Select_New);
          when 36 =>
            -- Print
            Mng.Print;
          when 37 =>
            -- Switch francs/euros
            Mng.Change_Unit;
          when 38 =>
            -- Sort by date
            Mng.Sort (By_Date => True);
          when 39 =>
            -- Sort by abs(amount)
            Mng.Sort (By_Date => False);
          when 40 =>
            -- Exit
            Quit;
          when others =>
            Screen.Ack_Error(Screen.Internal_Error);
            Mng.Save(Mng.Rescue);
        end case;
      when Afpx.Refresh =>
        Redisplay := True;
      when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
         | Afpx.Wakeup_Event =>
        null;
    end case;
  end loop;

exception
  when Usage_Error =>
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                    & "[ -e | -f ] [ <account_file> ]");
    Ada.Text_Io.Put_Line ("  (default is euros)");
  when Quit_Program =>
    Con_Io.Destroy;
  when Ooops : others =>
    begin
      Screen.Reset;
      Screen.Ack_Error(Screen.Internal_Error);
    exception
      when others => null;
    end;
    begin
      Ada.Text_Io.Put_Line("Exception: " & Ada.Exceptions.Exception_Name(Ooops));
    exception
      when others => null;
    end;
    Mng.Save(Mng.Rescue);
end Account; 

