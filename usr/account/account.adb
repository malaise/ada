-- Usage: account [ -e | -f ] [ <file> ]
--  (default is euros) 
with TEXT_IO, ADA.EXCEPTIONS;
with ARGUMENT, CON_IO, AFPX;
with UNIT_FORMAT, OPER_DEF, SCREEN, MNG;
procedure ACCOUNT is

  -- Afpx put_then_get stuff
  CURSOR_FIELD : AFPX.ABSOLUTE_FIELD_RANGE := 1;
  CURSOR_COL   : CON_IO.COL_RANGE := 0;
  PTG_RESULT   : AFPX.RESULT_REC;
  REDISPLAY    : BOOLEAN := FALSE;


  -- Parsing
  USAGE_ERROR : exception;

  procedure SET_UNIT (STR : in STRING) is
  begin
    if STR = "-f" then
      UNIT_FORMAT.SET_UNIT_TO(UNIT_FORMAT.FRANCS);
    elsif STR = "-e" then
      UNIT_FORMAT.SET_UNIT_TO(UNIT_FORMAT.EUROS);
    else
      raise USAGE_ERROR;
    end if;
  end SET_UNIT;

  -- We have to quit program: raises QUIT_PROGRAM
  QUIT_PROGRAM : exception;
  procedure QUIT is separate;
    
begin

  -- Check arguments
  -- Screen.reset must be called before mng.load(which uses the screen),
  --  but after sanity checks (which don't the screen to be loaded)
  declare
    FILE_ARG : NATURAL := 0;
  begin
    if ARGUMENT.GET_NBRE_ARG = 0 then
      null;
    elsif ARGUMENT.GET_NBRE_ARG = 1 then
      if ARGUMENT.GET_PARAMETER = "-h" then
        raise USAGE_ERROR;
      end if;
      begin
        -- Try to set unit
        SET_UNIT(ARGUMENT.GET_PARAMETER);
      exception
        when USAGE_ERROR =>
          -- Not valid unit mode -> file
          FILE_ARG := 1;
      end;
    elsif ARGUMENT.GET_NBRE_ARG = 2 then 
      SET_UNIT(ARGUMENT.GET_PARAMETER(1));
      FILE_ARG := 2;
    else
      raise USAGE_ERROR;
    end if;

    -- Init the screen
    SCREEN.RESET;

    -- No data
    MNG.CLEAR;

    -- Load file if any
    if FILE_ARG /= 0 then
      MNG.LOAD(ARGUMENT.GET_PARAMETER(FILE_ARG));
    end if;
  end;

  -- Now the main loop
  loop
    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT, REDISPLAY);
    MNG.SET_CURRENT (PTG_RESULT.ID_SELECTED);
    REDISPLAY := FALSE;
    case PTG_RESULT.EVENT is
      when AFPX.KEYBOARD =>
        case PTG_RESULT.KEYBOARD_KEY is
          when AFPX.RETURN_KEY =>
            null;
          when AFPX.ESCAPE_KEY =>
            QUIT;
          when AFPX.BREAK_KEY =>
            QUIT;
        end case;
      when AFPX.MOUSE_BUTTON =>
        case PTG_RESULT.FIELD_NO is
          -- Double click in list
          when AFPX.LIST_FIELD_NO =>
            MNG.UPDATE_STATE;

          -- List movements
          when 17 =>
            -- Top
            AFPX.UPDATE_LIST(AFPX.TOP);
          when 18 =>
            -- PgUp
            AFPX.UPDATE_LIST(AFPX.PAGE_UP);
          when 19 =>
            -- Up
            AFPX.UPDATE_LIST(AFPX.UP);
          when 20 =>
            -- Down
            AFPX.UPDATE_LIST(AFPX.DOWN);
          when 21 =>
            -- PgDown
            AFPX.UPDATE_LIST(AFPX.PAGE_DOWN);
          when 22 =>
            -- Bottom
            AFPX.UPDATE_LIST(AFPX.BOTTOM);

          -- Oper actions
          when 24 =>
            -- New
            MNG.ADD_OPER;
          when 25 =>
            -- Edit
            MNG.EDIT_OPER;
          when 26 =>
            -- View
            MNG.VIEW_OPER;
          when 27 =>
            -- Delete
            MNG.DEL_OPER;
          when 28 =>
            -- Update
            MNG.GARBAGE_COLLECT;
          when 29 =>
            -- Search
            MNG.SEARCH;
          when 30 =>
            -- Show all
            MNG.SHOW_ALL;

          -- Account actions
          when 32 =>
            -- Create
            MNG.CLEAR;
          when 33 =>
            -- Load
            MNG.LOAD("");
          when 34 =>
            -- Save
            MNG.SAVE;
          when 35 =>
            -- Print
            MNG.PRINT;
          when 36 =>
            -- Switch francs/euros
            MNG.CHANGE_UNIT;
          when 37 =>
            -- Sort
            MNG.SORT;
          when 38 =>
            -- Exit
            QUIT;
          when others =>
            SCREEN.ACK_ERROR(SCREEN.INTERNAL_ERROR);
            MNG.SAVE(RESCUE => TRUE);
        end case;
      when AFPX.REFRESH =>
        REDISPLAY := TRUE;
    end case;
  end loop;

exception
  when USAGE_ERROR =>
    TEXT_IO.PUT_LINE ("Usage: " & ARGUMENT.GET_PROGRAM_NAME
                    & "[ -e | -f ] [ <account_file> ]");
    TEXT_IO.PUT_LINE ("  (default is euros)");
  when QUIT_PROGRAM =>
    CON_IO.DESTROY;
  when OOOPS : others =>
    SCREEN.RESET;
    SCREEN.ACK_ERROR(SCREEN.INTERNAL_ERROR);
    TEXT_IO.PUT_LINE("Exception: " & ADA.EXCEPTIONS.EXCEPTION_NAME(OOOPS));
    MNG.SAVE(RESCUE => TRUE);
end ACCOUNT; 

