with CALENDAR; use CALENDAR;
with CON_IO, TIMERS;
with SOK_DISPLAY, SOK_TIME;
package body SOK_INPUT is

  PLAY : BOOLEAN := TRUE;
  DELTA_GET : constant CON_IO.DELAY_REC(TIMERS.DELAY_SEC) 
            := (DELAY_KIND => TIMERS.DELAY_SEC,
                PERIOD => CON_IO.NO_PERIOD,
                DELAY_SECONDS => 1.0);

  function GET_KEY return KEY_LIST is
    STR  : STRING (1 .. 1);
    LAST : NATURAL;
    STAT : CON_IO.CURS_MVT;
    POS  : POSITIVE;
    INS  : BOOLEAN;
    
    use CON_IO;
  begin
    loop
      if PLAY then
        SOK_TIME.DISP_TIME;
      end if;
      CON_IO.GET (STR, LAST, STAT, POS, INS,
                  TIME_OUT => DELTA_GET,
                  ECHO     => FALSE);
      if PLAY then
        case STAT is
          when UP => return UP;
          when DOWN => return DOWN;
          when PGUP | PGDOWN | CTRL_PGUP | CTRL_PGDOWN => null;
          when LEFT => return LEFT;
          when RIGHT => return RIGHT;
          when FULL =>
            case STR(1) is
              when 'u' | 'U' =>
                return UNDO;
              when 'w' | 'W' =>
                PLAY := not PLAY;
                CON_IO.CLEAR;
                SOK_TIME.STOP_TIME;
              when ' '  =>
                return NEXT;
              when others => null;
            end case; -- on char
          when TAB | STAB  => null;
          when RET => return NEXT;
          when ESC => return ESC;
          when BREAK => raise BREAK_REQUESTED;
          when MOUSE_BUTTON => null;
          when TIMEOUT => null;
          when FD_EVENT | TIMER_EVENT => null;
          when REFRESH =>
            return REFRESH;
        end case;
      else
        if STAT = FULL
        and then (STR(1) = 'w' or else STR(1) = 'W') then
          PLAY := not PLAY;
          SOK_TIME.START_TIME;
          return REFRESH;
        end if;
      end if;
    end loop;
  end GET_KEY;

  procedure PAUSE is
    STR  : STRING (1 .. 1);
    LAST : NATURAL;
    STAT : CON_IO.CURS_MVT;
    POS  : POSITIVE;
    INS  : BOOLEAN;
    
    use CON_IO;
  begin
    loop
      CON_IO.GET (STR, LAST, STAT, POS, INS,
                  TIME_OUT => DELTA_GET,
                  ECHO     => FALSE);
      case STAT is
        when UP => return;
        when DOWN => return;
        when PGUP | PGDOWN | CTRL_PGUP | CTRL_PGDOWN => null;
        when LEFT => return;
        when RIGHT => return;
        when FULL => return;
        when TAB | STAB  => null;
        when RET => return;
        when ESC => return;
        when BREAK => raise BREAK_REQUESTED;
        when MOUSE_BUTTON => null;
        when TIMEOUT => null;
        when FD_EVENT | TIMER_EVENT => null;
        when REFRESH => null;
      end case;
    end loop;
  end PAUSE;

  procedure END_OF_PROGRAM is
  begin
    null;
  end END_OF_PROGRAM;

end SOK_INPUT;
