with CALENDAR;
with MY_IO, NORMAL;
with GENERIC_CON_IO;
with ARGUMENT;

procedure T_CON is

  task type TASK_T;
  T1, T2 : TASK_T;

  task body TASK_T is

    package CON_IO is new GENERIC_CON_IO.ONE_CON_IO(1);
    use CON_IO;

    W1, W2, W3 : WINDOW;
    STR : STRING (1..25);
    LAST : NATURAL;
    COL : NATURAL;
    STAT : CURS_MVT;
    STR_EXIT : constant STRING := "exit";
    WIDTH : NATURAL;
    DELT : constant CON_IO.DELAY_REC(CON_IO.DELAY_SEC)
         := (DELAY_KIND => CON_IO.DELAY_SEC, DELAY_SECONDS => 10.0);
    POS : POSITIVE;
    INS : BOOLEAN;
    T0  : constant CALENDAR.DAY_DURATION
        := CALENDAR.SECONDS(CALENDAR.CLOCK);

    MOUSE_EVENT : CON_IO.MOUSE_EVENT_REC;

    procedure SHOW_CLOCK is
      T : NATURAL;
    begin
      MOVE (0, 0, W3);
      T := NATURAL(CALENDAR.SECONDS(CALENDAR.CLOCK) - T0);
      PUT (NATURAL'IMAGE(T) & "   ", W3);
    end SHOW_CLOCK;

  begin
    begin
      COL := 1;
      WIDTH := 20;
      COL := NATURAL'VALUE (ARGUMENT.GET_PARAMETER (1));
      WIDTH := NATURAL'VALUE (ARGUMENT.GET_PARAMETER (2));
    exception
      when ARGUMENT.ARGUMENT_NOT_FOUND => null;
    end;

    INIT;
    RESET_TERM;
    ENABLE_MOTION_EVENTS(TRUE);
    -- fenetre de saisie, fenetre d'affichage
    OPEN ( W1, ( 5, 15), (10, 78));
    OPEN ( W2, (15,  1), (17, 78));
    OPEN ( W3, (20,  0), (20, 9));

    SET_FOREGROUND (LIGHT_BLUE, NOT_BLINK, W1);
    SET_FOREGROUND (CYAN, NOT_BLINK, W2);
    SET_FOREGROUND (LIGHT_GREEN, NOT_BLINK, W3);

    SET_BACKGROUND (BLUE, W1);
    SET_BACKGROUND (RED, W2);
    SET_BACKGROUND (GREEN, W3);

    CLEAR (W1);
    CLEAR (W2);
    CLEAR (W3);

    FRAME(NAME => W2);

    SHOW_CLOCK;
    MOVE (1, COL, W1);
    GET (STR(1..WIDTH), LAST, STAT, POS, INS,
       W1, CURRENT, CURRENT, RED, DELT);
    loop
        CLEAR (W2);
        PUT (">" & STR(1..LAST) & "<" & CURS_MVT'IMAGE(STAT), W2 );
        if STAT = ESC then
          STR (1 .. WIDTH) := (others => ' ');
          POS := 1;
          INS := FALSE;
        elsif STAT = BREAK then
          exit;
        elsif STAT = RET then
          if STR (1..LAST) = STR_EXIT then
            exit;
          end if;
        elsif STAT = MOUSE_BUTTON then
          CON_IO.GET_MOUSE_EVENT (MOUSE_EVENT);   
          if MOUSE_EVENT.VALID then
            PUT (" T", W2);
          else
            PUT (" D", W2);
          end if;
          if MOUSE_EVENT.STATUS = PRESSED then
            PUT (" P", W2);
          elsif MOUSE_EVENT.STATUS = RELEASED then
            PUT (" R", W2);
          elsif MOUSE_EVENT.STATUS = MOTION then
            PUT (" M", W2);
          end if;
          if MOUSE_EVENT.BUTTON = LEFT then
            PUT (" L", W2);
          elsif MOUSE_EVENT.BUTTON = MIDDLE then
            PUT (" M", W2);
          elsif MOUSE_EVENT.BUTTON = RIGHT then
            PUT (" R", W2);
          elsif MOUSE_EVENT.BUTTON = MOTION then
            PUT (" x", W2);
          end if;
          PUT (NORMAL(MOUSE_EVENT.ROW, 4) & NORMAL(MOUSE_EVENT.COL, 4), W2);
          if MOUSE_EVENT.VALID
          and then MOUSE_EVENT.STATUS = PRESSED
          and then MOUSE_EVENT.BUTTON = LEFT
          and then MOUSE_EVENT.ROW = ROW_RANGE'FIRST
          and then MOUSE_EVENT.COL = COL_RANGE'FIRST then
            exit;
          end if;
        end if;
        SHOW_CLOCK;
        MOVE (1, COL, W1);
        PUT_THEN_GET(STR(1..WIDTH), LAST, STAT, POS, INS,
         W1, CURRENT, CURRENT, RED, DELT);
    end loop;


    ENABLE_MOTION_EVENTS(FALSE);
    for I in 1 .. 3 loop
      CLEAR (W1);
      MOVE (6 - I, 2, W1);
      PUT ("Exiting", W1, RED, BLINK, GREEN);
      GET (STR(1..0), LAST, STAT, POS, INS,
         W1, CURRENT, CURRENT, RED,
         (DELAY_KIND => CON_IO.DELAY_SEC, DELAY_SECONDS => 3.0) );
    end loop;

    DESTROY;
    delay 3.0;

    INIT;
    delay 2.0;
    DESTROY;
    delay 1.0;


  exception
    when others =>
       MY_IO.PUT_LINE ("Exception");
       raise;
  end TASK_T;


begin
  null;
end T_CON;
