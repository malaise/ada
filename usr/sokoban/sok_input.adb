with CALENDAR; use CALENDAR;
with CON_IO;
with SOK_DISPLAY, SOK_TIME;
package body SOK_INPUT is

  PLAY : BOOLEAN := TRUE;
  DELTA_GET : constant CON_IO.DELAY_REC(CON_IO.DELAY_SEC) 
            := (DELAY_KIND => CON_IO.DELAY_SEC, DELAY_SECONDS => 1.0);

  function GET_KEY return KEY_LIST is
    EVENT : CON_IO.EVENT_LIST;
    KEY : NATURAL;
    IS_CHAR, CTRL, SHIFT : BOOLEAN;
    CHAR : CHARACTER;
    use CON_IO;
  begin
    loop
      if PLAY then
        SOK_TIME.DISP_TIME;
      end if;
      CON_IO.GET_KEY_TIME (TRUE, EVENT, KEY, IS_CHAR, CTRL, SHIFT, DELTA_GET);
      if EVENT = CON_IO.ESC then
        if not IS_CHAR and then PLAY then  
          case KEY is
            when 16#52# => return UP;
            when 16#51# => return LEFT;
            when 16#53# => return RIGHT;
            when 16#54# => return DOWN;
            when 16#0D# => return NEXT;
            when 16#1B# => return ESC;
            when 16#08# => return UNDO;
            when others => null;
          end case;
        else
          begin
            CHAR := CHARACTER'VAL (KEY);
          exception
            -- values can be more than 127
            when CONSTRAINT_ERROR => CHAR := ASCII.NUL;
          end;

          case CHAR is
            when 'u' | 'U' =>
              if PLAY then
                return UNDO;
              end if;
            when 'w' | 'W' =>
              if PLAY then
                PLAY := not PLAY;
                CON_IO.CLEAR;
                SOK_TIME.STOP_TIME;
              else
                PLAY := not PLAY;
                SOK_TIME.START_TIME;
                return REFRESH;
              end if;
            when ' '  =>
              if PLAY then
                return NEXT;
              end if;
            when others => null;
          end case; -- on char
        end if; -- is_char
      elsif EVENT = CON_IO.BREAK then
        raise BREAK_REQUESTED;
      elsif EVENT = CON_IO.REFRESH and then PLAY then
        return REFRESH;
      end if; -- event=esc
    end loop;
  end GET_KEY;

  procedure PAUSE is
    EVENT : CON_IO.EVENT_LIST;
    KEY : NATURAL;
    IS_CHAR, CTRL, SHIFT : BOOLEAN;
    use CON_IO;
  begin
    loop
      CON_IO.GET_KEY_TIME (TRUE, EVENT, KEY, IS_CHAR, CTRL, SHIFT, DELTA_GET);
      exit when EVENT = CON_IO.ESC or else EVENT = CON_IO.BREAK;
    end loop;
  end PAUSE;

  procedure END_OF_PROGRAM is
  begin
    null;
  end END_OF_PROGRAM;

end SOK_INPUT;
