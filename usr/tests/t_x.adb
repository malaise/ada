with MY_IO, X_MNG, ARGUMENT, TEXT_HANDLER;
use X_MNG;
procedure T_X is

  LINE_DEF : constant X_MNG.LINE_DEFINITION_REC := (
    SCREEN_ID => 0,
    ROW => 10,
    COLUMN => 20,
    HEIGHT => 50,
    WIDTH => 80,
    BACKGROUND => 0,
    BORDER => 1,
    NO_FONT => 1);

  ID : X_MNG.LINE;

  TIMEOUT_MS, VAR_TIMEOUT_MS : INTEGER;
  X_EVENT : BOOLEAN;
  KIND : X_MNG.EVENT_KIND;
  NEXT : BOOLEAN;

  KBD_CODES : X_MNG.KBD_TAB_CODE;
  TID_BUTTON : X_MNG.BUTTON_LIST;
  TID_ROW, TID_COL : INTEGER;

  TXT : TEXT_HANDLER.TEXT(80);
  subtype ROW_RANGE is NATURAL range 10 .. 30;
  ROW : ROW_RANGE := ROW_RANGE'FIRST;

  procedure PUT (STR : in STRING) is
  begin
    X_MNG.X_PUT_STRING (ID, " ", ROW, 8);
    if ROW /= ROW_RANGE'LAST then
      ROW := ROW_RANGE'SUCC(ROW);
    else
      ROW := ROW_RANGE'FIRST;
    end if;
    X_MNG.X_PUT_STRING (ID, "> " & STR & "                       ",
                            ROW, 8);
  end PUT;

begin
  if ARGUMENT.GET_NBRE_ARG = 0 then
    X_MNG.X_INITIALISE ("");
    TIMEOUT_MS := 1_000;
  elsif ARGUMENT.GET_NBRE_ARG = 1 then
    X_MNG.X_INITIALISE (ARGUMENT.GET_PARAMETER(1));
    TIMEOUT_MS := 1_000;
  elsif ARGUMENT.GET_NBRE_ARG = 2 then
    X_MNG.X_INITIALISE (ARGUMENT.GET_PARAMETER(1));
    TIMEOUT_MS := INTEGER'VALUE (ARGUMENT.GET_PARAMETER(2));
  end if;

  X_MNG.X_OPEN_LINE (LINE_DEF, ID);

  X_MNG.X_SET_ATTRIBUTES (ID, 0, 5, TRUE, FALSE, FALSE, FALSE);
  for I in 0 .. 15 loop
    for J in 0 .. 15 loop
      X_MNG.X_PUT_CHAR (ID, X_MNG.BYTE(16 * I + J), 8 + I, 60 + J);
    end loop;
  end loop;

  X_EVENT := TRUE;
  KIND := X_MNG.REFRESH;
  MAIN_LOOP:
  loop
    if X_EVENT and then KIND = X_MNG.REFRESH then
      X_MNG.X_SET_ATTRIBUTES (ID, 0, 3, FALSE, FALSE, TRUE, FALSE);
      X_MNG.X_PUT_STRING (ID, "Ah que coucou", 5, 10);
      X_MNG.X_SET_ATTRIBUTES (ID, 1, 4);
      X_MNG.X_DRAW_AREA (ID, 50, 2, 7, 10);
      X_MNG.X_SET_ATTRIBUTES (ID, 0, 3, FALSE, FALSE, FALSE, FALSE);
      X_MNG.X_BELL (ID, 1);
    end if;
    VAR_TIMEOUT_MS := TIMEOUT_MS;
    X_MNG.X_SELECT (ID, VAR_TIMEOUT_MS, X_EVENT);
    if X_EVENT then
      loop
        X_MNG.X_PROCESS_EVENT (ID, KIND, NEXT);
        case KIND is
          when X_MNG.DISCARD =>
            PUT (X_MNG.EVENT_KIND'IMAGE(KIND));
          when X_MNG.REFRESH =>
            X_MNG.X_SET_ATTRIBUTES (ID, 0, 3, FALSE, FALSE, TRUE, FALSE);
            PUT (X_MNG.EVENT_KIND'IMAGE(KIND));
          when X_MNG.FD_EVENT =>
            X_MNG.X_SET_ATTRIBUTES (ID, 0, 3, FALSE, FALSE, TRUE, FALSE);
          when X_MNG.TID_PRESS | X_MNG.TID_RELEASE =>
            X_MNG.X_READ_TID (ID, TRUE, TID_BUTTON, TID_ROW, TID_COL);
            PUT (X_MNG.EVENT_KIND'IMAGE(KIND) & " " & X_MNG.BUTTON_LIST'IMAGE(TID_BUTTON)
                              & " " & INTEGER'IMAGE(TID_ROW)  & " " & INTEGER'IMAGE(TID_COL));
            exit MAIN_LOOP when TID_ROW = 1 and then TID_COL = 1;
          when X_MNG.TID_MOTION =>
            null;
          when X_MNG.KEYBOARD =>
            X_MNG.X_READ_KEY(ID, KBD_CODES);
            TEXT_HANDLER.SET (TXT, X_MNG.EVENT_KIND'IMAGE(KIND));
            for I in 1 .. KBD_CODES.NBRE loop
               TEXT_HANDLER.APPEND (TXT, " " & X_MNG.BYTE'IMAGE(KBD_CODES.TAB(I)));
            end loop;
            PUT (TEXT_HANDLER.VALUE(TXT));
            exit MAIN_LOOP when KBD_CODES.NBRE = 2
                 and then KBD_CODES.TAB(1) = 255 and then KBD_CODES.TAB(2) = 27;
          end case;
        exit when not NEXT;
      end loop;
    else
--      PUT ("Timeout");
null;
    end if;
    MY_IO.PUT_LINE (INTEGER'IMAGE(VAR_TIMEOUT_MS));
  end loop MAIN_LOOP;

  X_MNG.X_CLOSE_LINE (ID);
end T_X;
