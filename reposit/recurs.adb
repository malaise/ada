with MY_IO, TEXT_HANDLER, DIRECTORY;
procedure RECURS (
 NAME_OF_DIR : in BOOLEAN := TRUE;
 IN_CURRENT : in BOOLEAN := TRUE;
 FIRST_LEVEL_ONLY : in BOOLEAN := FALSE;
 LEAVES_ONLY : in BOOLEAN := FALSE;
 STOP_ON_ERROR : in BOOLEAN := TRUE) is

  CURRENT_LEVEL : NATURAL := 0;
  ABORT_EXPLORE : exception;
  DOT_DIR     : constant STRING := ".";
  DOT_DOT_DIR : constant STRING := "..";

  subtype DIR_TXT is TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN);

  procedure EXPLORE (CURR_NAME : in STRING) is
    DIR_DSC : DIRECTORY.DIR_DESC;
    FULL_CURR_NAME, NEW_NAME : DIR_TXT;
    KIND : DIRECTORY.FILE_KIND_LIST;
    RIGHTS : NATURAL;
    MTIME : DIRECTORY.TIME_T;
    NB_SONS : NATURAL;
    use DIRECTORY;

    procedure DO_HERE is
    begin
      -- Display current drive and directory
      if NAME_OF_DIR then
        MY_IO.NEW_LINE;
        MY_IO.PUT ("==> ");
        MY_IO.PUT (TEXT_HANDLER.VALUE(FULL_CURR_NAME));
        MY_IO.PUT_LINE (" <==");
      end if;

      if not DO_IN_DIR and then STOP_ON_ERROR then
        MY_IO.PUT_LINE (" *** Abort ***");
        raise ABORT_EXPLORE;
      end if;
    end DO_HERE;

  begin

    -- Go in current directory
    begin
      DIRECTORY.CHANGE_CURRENT (CURR_NAME);
    exception
      when DIRECTORY.NAME_ERROR =>
        MY_IO.NEW_LINE;
        MY_IO.PUT ("Error changing to directory " & CURR_NAME);
        if STOP_ON_ERROR then
          MY_IO.PUT_LINE (" *** Abort ***");
          raise ABORT_EXPLORE;
        else
           MY_IO.NEW_LINE;
           return;
        end if;
    end;

    DIRECTORY.GET_CURRENT (FULL_CURR_NAME);

    -- Do current dir when not LEAVES_ONLY
    if not LEAVES_ONLY then
      if CURRENT_LEVEL /= 0 or else IN_CURRENT then
        -- Check if do action in intial dir
        DO_HERE;
      end if;
      -- Optim: Done it if first level only
      if CURRENT_LEVEL = 1 and then FIRST_LEVEL_ONLY then
        return;
      end if;
    end if;

    -- Go to next sub dir
    NB_SONS := 0;
    DIR_DSC := DIRECTORY.OPEN(TEXT_HANDLER.VALUE(FULL_CURR_NAME));
    loop
      begin
        DIRECTORY.NEXT_ENTRY (DIR_DSC, NEW_NAME);
      exception
        when DIRECTORY.END_ERROR =>
          exit;
      end;
      begin
        DIRECTORY.FILE_STAT (TEXT_HANDLER.VALUE(NEW_NAME), KIND, RIGHTS, MTIME);
      exception
        when DIRECTORY.NAME_ERROR =>
          -- A link to nowhere?
          KIND := DIRECTORY.UNKNOWN;
      end;
      if KIND = DIRECTORY.DIR
      and then TEXT_HANDLER.VALUE(NEW_NAME) /= DOT_DIR 
      and then TEXT_HANDLER.VALUE(NEW_NAME) /= DOT_DOT_DIR then
        -- Restart with next son if not FIRST_LEVEL_ONLY
        if CURRENT_LEVEL /= 1 or else not FIRST_LEVEL_ONLY then
          CURRENT_LEVEL := CURRENT_LEVEL + 1;
          EXPLORE (TEXT_HANDLER.VALUE(NEW_NAME));
          CURRENT_LEVEL := CURRENT_LEVEL - 1;
          DIRECTORY.CHANGE_CURRENT (TEXT_HANDLER.VALUE(FULL_CURR_NAME));
        end if;
        NB_SONS := NB_SONS + 1;
      end if;
    end loop;
    DIRECTORY.CLOSE(DIR_DSC);


    -- When LEAVES_ONLY, do current dir after counting sons
    if NB_SONS = 0 and then LEAVES_ONLY then
      -- Check if do action in current dir
      if CURRENT_LEVEL /= 0 or else IN_CURRENT then
        DO_HERE;
      end if;
    end if;

  exception
    when ABORT_EXPLORE =>
      if CURRENT_LEVEL = 0 then
        -- Back to start directory
        DIRECTORY.CHANGE_CURRENT (TEXT_HANDLER.VALUE(FULL_CURR_NAME));
      else
        CURRENT_LEVEL := CURRENT_LEVEL - 1;
        raise;
      end if;
  end EXPLORE;

begin -- RECURS
  EXPLORE (DIRECTORY.GET_CURRENT);
end recurs;
