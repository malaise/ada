with CALENDAR;
with ARGUMENT, AFPX, CON_IO, DYNAMIC_LIST, DIR_MNG, NORMAL, DOS;
procedure T_SCREEN is

  AFPX_ITEM : AFPX.LINE_REC;

  DSCR : AFPX.DESCRIPTOR_RANGE;

  CURSOR_FIELD : AFPX.FIELD_RANGE;
  CURSOR_COL   : CON_IO.COL_RANGE;
  PTG_RESULT   : AFPX.RESULT_REC;
  REDISPLAY : BOOLEAN;


  CURR_DATE  : AFPX.ABSOLUTE_FIELD_RANGE;

  EXIT_BUTTON : AFPX.ABSOLUTE_FIELD_RANGE;
  FIRST_GET   : AFPX.ABSOLUTE_FIELD_RANGE;


  LIST_EMPTY : BOOLEAN;

  ALLOW_UNDO  : BOOLEAN;
  ALLOW_DRAW  : BOOLEAN;
  IN_ADD : BOOLEAN;

  IN_EDIT  : BOOLEAN;
  IN_VALID : BOOLEAN;


  ACT : BOOLEAN;

  use AFPX;
begin
  if ARGUMENT.GET_NBRE_ARG < 1 then
    return;
  end if;

  DSCR := AFPX.DESCRIPTOR_RANGE'VALUE (ARGUMENT.GET_PARAMETER);

  declare
    procedure DIR_SORT is new DIR_MNG.FILE_LIST_MNG.SORT (DIR_MNG.LESS_THAN);
    DIR_LIST : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
    DIR_ITEM : DIR_MNG.FILE_ENTRY_REC;
  begin
    -- List directory and store it in AFPX list
    if ARGUMENT.GET_NBRE_ARG = 1 then
      DIR_MNG.LIST_DIR (DIR_LIST, "");
    else
      DIR_MNG.LIST_DIR (DIR_LIST,
                        ARGUMENT.GET_PARAMETER (OCCURENCE => 2));
    end if;

    -- Sort, move to first, copy in afpx list
    DIR_SORT (DIR_LIST);
    DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST, DIR_MNG.FILE_LIST_MNG.NEXT,
                                  0 , FALSE);
    loop
      DIR_MNG.FILE_LIST_MNG.READ (DIR_LIST, DIR_ITEM,
                                  DIR_MNG.FILE_LIST_MNG.CURRENT);
      AFPX_ITEM.LEN := DIR_ITEM.LEN;
      AFPX_ITEM.STR := (others => ' ');
      AFPX_ITEM.STR(1 .. AFPX_ITEM.LEN) := DIR_ITEM.NAME (1 .. DIR_ITEM.LEN);
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST, AFPX_ITEM);
      exit when DIR_MNG.FILE_LIST_MNG.GET_POSITION (DIR_LIST)
              = DIR_MNG.FILE_LIST_MNG.LIST_LENGTH (DIR_LIST);
      DIR_MNG.FILE_LIST_MNG.MOVE_TO (DIR_LIST);
    end loop;
    -- End of list
    AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.PREV,
                                0, FALSE);
  exception
    when DIR_MNG.NAME_ERROR =>
      null;
  end;

  AFPX.USE_DESCRIPTOR(DSCR);

  if DSCR = 1 then
    EXIT_BUTTON  := 19;
    FIRST_GET := 07;
    ALLOW_UNDO := FALSE;
    CURR_DATE := 02;
  elsif DSCR = 2 then
    EXIT_BUTTON  := 5;
    FIRST_GET := 11;
    IN_EDIT := FALSE;
    IN_VALID := FALSE;
    CURR_DATE := 02;
  elsif DSCR = 3 then
    EXIT_BUTTON := 125;
    FIRST_GET := 04;
    IN_EDIT := TRUE;
    CURR_DATE := 02;
  end if;

  CURSOR_FIELD := FIRST_GET;
  CURSOR_COL := 0;
  REDISPLAY := FALSE;


  -- Encode date
  declare
    CURRENT_TIME : constant CALENDAR.TIME := CALENDAR.CLOCK;
  begin
    AFPX.ENCODE_FIELD (CURR_DATE, (0, 0),
      NORMAL(CALENDAR.DAY(CURRENT_TIME)  , 2, GAP => '0') & "/"
    & NORMAL(CALENDAR.MONTH(CURRENT_TIME), 2, GAP => '0') & "/"
    & NORMAL(CALENDAR.YEAR(CURRENT_TIME),  4, GAP => '0') );
  end;


  loop

    LIST_EMPTY := AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST) = 0;
    if DSCR = 1 then
      ALLOW_DRAW := not LIST_EMPTY and then
                    AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST) <= 10;
      AFPX.SET_FIELD_ACTIVATION (22, not LIST_EMPTY);
      AFPX.SET_FIELD_ACTIVATION (23, ALLOW_DRAW);
      AFPX.SET_FIELD_ACTIVATION (24, not LIST_EMPTY);
      AFPX.SET_FIELD_ACTIVATION (26, not LIST_EMPTY);
      AFPX.SET_FIELD_ACTIVATION (27, not LIST_EMPTY);
      AFPX.SET_FIELD_ACTIVATION (17, ALLOW_UNDO);
      AFPX.ENCODE_FIELD (20, (0, 0),
        NORMAL(AFPX.LINE_LIST_MNG.LIST_LENGTH(AFPX.LINE_LIST), 5) );
    elsif DSCR = 2 then
      -- List and menu buttons, not in valid nor edit
      -- Same for create
      ACT := not IN_VALID and then not IN_EDIT;
      AFPX.SET_FIELD_ACTIVATION (00, ACT);
      AFPX.SET_FIELD_ACTIVATION (03, ACT);
      AFPX.SET_FIELD_ACTIVATION (04, ACT);
      AFPX.SET_FIELD_ACTIVATION (05, ACT);
      AFPX.SET_FIELD_ACTIVATION (06, ACT);
      -- Delete/edit if not empty and not in valid nor edit
      ACT := ACT and then not LIST_EMPTY;
      AFPX.SET_FIELD_ACTIVATION (07, ACT);
      AFPX.SET_FIELD_ACTIVATION (08, ACT);
      -- Edit if edit
      ACT := IN_EDIT or else IN_VALID;
      for I in AFPX.FIELD_RANGE'(10) .. 23 loop
        AFPX.SET_FIELD_ACTIVATION (I, ACT);
      end loop;
      -- Un protect get in edit
      ACT := not IN_EDIT;
      AFPX.SET_FIELD_PROTECTION (11, ACT);
      AFPX.SET_FIELD_PROTECTION (13, ACT);
      AFPX.SET_FIELD_PROTECTION (16, ACT);
      AFPX.SET_FIELD_PROTECTION (17, ACT);
      AFPX.SET_FIELD_PROTECTION (18, ACT);
      AFPX.SET_FIELD_PROTECTION (19, ACT);
      AFPX.SET_FIELD_PROTECTION (20, ACT);
      AFPX.SET_FIELD_PROTECTION (21, ACT);
      AFPX.SET_FIELD_ACTIVATION (24, not ACT);
      if ACT then
        AFPX.SET_FIELD_COLORS (11, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (13, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (16, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (17, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (18, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (19, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (20, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
        AFPX.SET_FIELD_COLORS (21, FOREGROUND => CON_IO.CYAN,
                                   BACKGROUND => CON_IO.BLACK);
      else
        AFPX.SET_FIELD_COLORS (11, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (13, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (16, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (17, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (18, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (19, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (20, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
        AFPX.SET_FIELD_COLORS (21, FOREGROUND => CON_IO.BROWN,
                                   BACKGROUND => CON_IO.BLUE);
      end if;
      -- Compute if in edit
      AFPX.SET_FIELD_ACTIVATION (24, IN_EDIT);
      -- Confirm if Valid
      AFPX.SET_FIELD_ACTIVATION (09, IN_VALID);
    elsif DSCR = 3 then
      null;
    end if;


    AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT);
    REDISPLAY := FALSE;

    case PTG_RESULT.EVENT is

      when REFRESH =>
        REDISPLAY := TRUE;

      when KEYBOARD =>

        case PTG_RESULT.KEYBOARD_KEY is
          when RETURN_KEY | BREAK_KEY =>
            null;
          when ESCAPE_KEY =>
            -- Clear current field
            AFPX.CLEAR_FIELD (CURSOR_FIELD);
            CURSOR_COL := 0;
            ALLOW_UNDO := FALSE;
        end case;

      when MOUSE_BUTTON =>

        exit when PTG_RESULT.FIELD_NO = EXIT_BUTTON;
        if DSCR = 1 then
          case PTG_RESULT.FIELD_NO is
            when 15 | 16 =>
              -- Add, rem select : confirm
              for I in AFPX.ABSOLUTE_FIELD_RANGE range 0 .. 27 loop
                AFPX.SET_FIELD_ACTIVATION (I, FALSE);
              end loop;
              AFPX.SET_FIELD_ACTIVATION (15, TRUE);
              AFPX.SET_FIELD_ACTIVATION (16, TRUE);
              AFPX.SET_FIELD_ACTIVATION (20, TRUE);
              if PTG_RESULT.FIELD_NO = 15 then
                IN_ADD := TRUE;
                AFPX.CLEAR_FIELD(16);
                AFPX.ENCODE_FIELD(16, (01, 04), "Abort");
                AFPX.ENCODE_FIELD(20, (00, 00),
                   "Add xxxxx records to the selection");
              else
                IN_ADD := FALSE;
                AFPX.CLEAR_FIELD(15);
                AFPX.ENCODE_FIELD(15, (01, 04), "Abort");
                AFPX.ENCODE_FIELD(20, (00, 00),
                   "Remove xxxxx records from the selection");
              end if;
              AFPX.PUT_THEN_GET (CURSOR_FIELD, CURSOR_COL, PTG_RESULT);
              -- Here we can confirm or abort
              if      (    IN_ADD and then PTG_RESULT.FIELD_NO = 15)
              or else (not IN_ADD and then PTG_RESULT.FIELD_NO = 16) then
                ALLOW_UNDO := TRUE;
              else
                ALLOW_UNDO := FALSE;
              end if;

              for I in AFPX.ABSOLUTE_FIELD_RANGE range 0 .. 27 loop
                AFPX.SET_FIELD_ACTIVATION (I, TRUE);
              end loop;
              AFPX.SET_FIELD_ACTIVATION (20, FALSE);
              AFPX.RESET_FIELD (15);
              AFPX.RESET_FIELD (16);
            when  22 | 27 =>
              -- Unselect, delete
              if AFPX.LINE_LIST_MNG.GET_POSITION (AFPX.LINE_LIST) /=
                 AFPX.LINE_LIST_MNG.LIST_LENGTH (AFPX.LINE_LIST) then
                AFPX.LINE_LIST_MNG.DELETE (AFPX.LINE_LIST);
              else
                AFPX.LINE_LIST_MNG.DELETE (AFPX.LINE_LIST,
                                           AFPX.LINE_LIST_MNG.PREV);
              end if;
            when others =>
              ALLOW_UNDO := FALSE;
          end case;
        elsif DSCR = 2 then
          case PTG_RESULT.FIELD_NO is
            when 06 | 08 =>
              -- Create, edit
              IN_EDIT := TRUE;
              IN_VALID := FALSE;
              CURSOR_FIELD := FIRST_GET;
            when 07 =>
              -- Delete
              IN_EDIT := FALSE;
              IN_VALID := TRUE;
            when 22 | 23 =>
              -- Valid, cancel
              IN_EDIT := FALSE;
              IN_VALID := FALSE;
            when others =>
              null;
          end case;
        elsif DSCR = 3 then
          null;
        end if;


    end case;

  end loop;

  CON_IO.RESET_TERM;

exception
  when others =>
    DOS.SOUND (3);
    delay 5.0;
    CON_IO.RESET_TERM;
    raise;
end T_SCREEN;
