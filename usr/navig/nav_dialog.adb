with CON_IO;
with NAV_TYPES;
with NAV_FORMAT;
-- Management of the dialog with the operator
package body NAV_DIALOG is

  -- remanent data across the get
  -- has the get (mask and data, and put) to be written at next get
  REFRESH : BOOLEAN;
  -- same for result
  RESULT_PUT : BOOLEAN;
  RESULT_DATA : NAV_DATA.T_DATA;

  -- is there an error
  CHK_ERROR : BOOLEAN;
  FMT_ERROR : BOOLEAN;

  -- lengths of strings
  LEN_SPEED : constant POSITIVE :=
   NAV_FORMAT.IMAG(RESULT_DATA.WIND.SPEED, FALSE)'LAST;
  LEN_ANGLE : constant POSITIVE :=
   NAV_FORMAT.IMAG(RESULT_DATA.WIND.ANGLE, FALSE)'LAST;
  LEN_DRIFT : constant POSITIVE :=
   NAV_FORMAT.IMAG(RESULT_DATA.DRIFT, FALSE)'LAST;


  -- to PUT all the data fields to get
  procedure PUT_DATA (DATA : in NAV_DATA.T_DATA) is
  begin
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.WIND_S,
     STR => NAV_FORMAT.IMAG(DATA.WIND.SPEED, DATA.SET(NAV_DATA.WIND_S) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.WIND_A,
     STR => NAV_FORMAT.IMAG(DATA.WIND.ANGLE, DATA.SET(NAV_DATA.WIND_A) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.PLAN_S,
     STR => NAV_FORMAT.IMAG(DATA.PLAN.SPEED, DATA.SET(NAV_DATA.PLAN_S) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.PLAN_A,
     STR => NAV_FORMAT.IMAG(DATA.PLAN.ANGLE, DATA.SET(NAV_DATA.PLAN_A) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.TRAJ_S,
     STR => NAV_FORMAT.IMAG(DATA.TRAJ.SPEED, DATA.SET(NAV_DATA.TRAJ_S) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.TRAJ_A,
     STR => NAV_FORMAT.IMAG(DATA.TRAJ.ANGLE, DATA.SET(NAV_DATA.TRAJ_A) ),
     BLINK => FALSE );
    NAV_SCREEN.PUT (
     FIELD => NAV_DATA.DRIFT,
     STR => NAV_FORMAT.IMAG(DATA.DRIFT, DATA.SET(NAV_DATA.DRIFT) ),
     BLINK => FALSE );
  end PUT_DATA;


  -- put the result
  procedure PUT (RESULT : in NAV_DATA.T_DATA) is
  begin
    RESULT_DATA := RESULT;
    RESULT_PUT := TRUE;
    for FIELD in NAV_DATA.T_LIST_DATA loop
      if RESULT.SET(FIELD) then
        NAV_SCREEN.DOT(FIELD);
      else
        NAV_SCREEN.ARROW(FIELD);
      end if;
    end loop;
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.WIND_S,
     STR  => NAV_FORMAT.IMAG(RESULT.WIND.SPEED, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.WIND_A,
     STR  => NAV_FORMAT.IMAG(RESULT.WIND.ANGLE, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.PLAN_S,
     STR  => NAV_FORMAT.IMAG(RESULT.PLAN.SPEED, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.PLAN_A,
     STR  => NAV_FORMAT.IMAG(RESULT.PLAN.ANGLE, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.TRAJ_S,
     STR  => NAV_FORMAT.IMAG(RESULT.TRAJ.SPEED, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.TRAJ_A,
     STR  => NAV_FORMAT.IMAG(RESULT.TRAJ.ANGLE, TRUE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.DRIFT,
     STR  => NAV_FORMAT.IMAG(RESULT.DRIFT, TRUE));
  end PUT;


  -- clear the result
  procedure CLEAR_RESULT is
  begin
    RESULT_PUT := FALSE;
    for FIELD in NAV_DATA.T_LIST_DATA loop
      NAV_SCREEN.CLEAR_LINE(FIELD);
    end loop;
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.WIND_S,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.WIND.SPEED, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.WIND_A,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.WIND.ANGLE, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.PLAN_S,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.PLAN.SPEED, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.PLAN_A,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.PLAN.ANGLE, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.TRAJ_S,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.TRAJ.SPEED, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.TRAJ_A,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.TRAJ.ANGLE, FALSE));
    NAV_SCREEN.PUT_RESULT (FIELD => NAV_DATA.DRIFT,
     STR  => NAV_FORMAT.IMAG(RESULT_DATA.DRIFT, FALSE));
  end CLEAR_RESULT;


  -- put consistency error message
  procedure PUT (ERROR : in NAV_DATA.T_CONSISTENCY) is
  begin
    CHK_ERROR := TRUE;
    FMT_ERROR := FALSE;
    NAV_SCREEN.ERR_CHECK(ERROR);
  end PUT;


  -- Inits the string according to a field a field
  procedure TO_STR (
   CURR_FIELD : in NAV_DATA.T_LIST_DATA;
   DATA       : in NAV_DATA.T_DATA;
   STR : out STRING;
   LEN : out POSITIVE) is
  begin
    case CURR_FIELD is
      when NAV_DATA.WIND_S =>
        LEN := LEN_SPEED;
        STR (1 .. LEN_SPEED) :=
         NAV_FORMAT.IMAG(DATA.WIND.SPEED, DATA.SET(NAV_DATA.WIND_S));
      when NAV_DATA.WIND_A =>
        LEN := LEN_ANGLE;
        STR (1 .. LEN_ANGLE) := 
         NAV_FORMAT.IMAG(DATA.WIND.ANGLE, DATA.SET(NAV_DATA.WIND_A));
      when NAV_DATA.PLAN_S =>
        LEN := LEN_SPEED;
        STR (1 .. LEN_SPEED) := 
         NAV_FORMAT.IMAG(DATA.PLAN.SPEED, DATA.SET(NAV_DATA.PLAN_S));
      when NAV_DATA.PLAN_A =>
        LEN := LEN_ANGLE;
        STR (1 .. LEN_ANGLE) :=
         NAV_FORMAT.IMAG(DATA.PLAN.ANGLE, DATA.SET(NAV_DATA.PLAN_A));
      when NAV_DATA.TRAJ_S =>
        LEN := LEN_SPEED;
        STR (1 .. LEN_SPEED) := 
         NAV_FORMAT.IMAG(DATA.TRAJ.SPEED, DATA.SET(NAV_DATA.TRAJ_S));
      when NAV_DATA.TRAJ_A =>
        LEN := LEN_ANGLE;
        STR (1 .. LEN_ANGLE) := 
         NAV_FORMAT.IMAG(DATA.TRAJ.ANGLE, DATA.SET(NAV_DATA.TRAJ_A));
      when NAV_DATA.DRIFT =>
        LEN := LEN_DRIFT;
        STR (1 .. LEN_DRIFT) :=
         NAV_FORMAT.IMAG(DATA.DRIFT, DATA.SET(NAV_DATA.DRIFT));
    end case;
  end TO_STR;


  -- Updates the data if string is correct
  procedure TO_VALUE (
   CURR_FIELD : in NAV_DATA.T_LIST_DATA;
   STR : in STRING;
   DATA : in out NAV_DATA.T_DATA;
   OK  : out BOOLEAN;
   POS : out POSITIVE) is
    SPEED : NAV_TYPES.T_SPEED;
    ANGLE : NAV_TYPES.T_ANGLE;
    DRIFT : NAV_TYPES.T_DRIFT;
    RES : NAV_FORMAT.FORMAT_RESULT;
    use NAV_FORMAT; -- for = tests
  begin

    -- get value and update data value if SET
    case CURR_FIELD is
      when NAV_DATA.WIND_S =>
        NAV_FORMAT.VALUE (STR, SPEED, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.WIND.SPEED := SPEED; end if;
      when NAV_DATA.WIND_A =>
        NAV_FORMAT.VALUE (STR, ANGLE, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.WIND.ANGLE := ANGLE; end if;
      when NAV_DATA.PLAN_S =>
        NAV_FORMAT.VALUE (STR, SPEED, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.PLAN.SPEED := SPEED; end if;
      when NAV_DATA.PLAN_A =>
        NAV_FORMAT.VALUE (STR, ANGLE, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.PLAN.ANGLE := ANGLE; end if;
      when NAV_DATA.TRAJ_S =>
        NAV_FORMAT.VALUE (STR, SPEED, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.TRAJ.SPEED := SPEED; end if;
      when NAV_DATA.TRAJ_A =>
        NAV_FORMAT.VALUE (STR, ANGLE, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.TRAJ.ANGLE := ANGLE; end if;
      when NAV_DATA.DRIFT =>
        NAV_FORMAT.VALUE (STR, DRIFT, RES, POS);
        if RES = NAV_FORMAT.SET then DATA.DRIFT := DRIFT; end if;
    end case;

    -- update data.set and ok
    case RES is
      when NAV_FORMAT.SET =>
        -- data already updated
        DATA.SET(CURR_FIELD) := TRUE;
        OK := TRUE;
      when NAV_FORMAT.UNSET =>
        DATA.SET(CURR_FIELD) := FALSE;
        OK := TRUE;
      when NAV_FORMAT.ERROR =>
        -- pos already set. data unchanged
        OK := FALSE;
    end case;

  exception
    when others =>
      POS := 1;
      OK := FALSE;
  end TO_VALUE;


  -- get data and then the action (compute or quit)
  procedure GET (DATA : in out NAV_DATA.T_DATA; TO_DO : out ACTION) is
    IN_ACTION : BOOLEAN;
    -- data field if not in action
    CURR_FIELD : NAV_DATA.T_LIST_DATA;
    -- ACTION position if in action
    CURR_ACTION : NAV_SCREEN.ACTION;
    -- if error
    BLINK : BOOLEAN;
    -- got string
    GET_STR : STRING (1..10);
    GET_LEN : POSITIVE;
    -- cursor position and insert status
    POS : POSITIVE;
    INS : BOOLEAN;
    -- result of try to convert string to a value : ok if set or unset
    OK : BOOLEAN;
    -- movement
    NXT : NAV_SCREEN.MOVEMENT;
    -- data at beginning of get
    DATA_IN : constant NAV_DATA.T_DATA := DATA;



  begin
    FMT_ERROR := FALSE;
    NXT := CON_IO.DOWN;
    -- go to first field. may be adapted in case of check error
    IN_ACTION := FALSE;
    CURR_FIELD := NAV_DATA.T_LIST_DATA'FIRST;

    loop
      if REFRESH then
        -- data part has to be put again
        NAV_SCREEN.TITLE;
        NAV_SCREEN.PUT_MASK;
        PUT_DATA (DATA);
        REFRESH := FALSE;
        if RESULT_PUT then
          -- result has to be put again
          PUT (RESULT_DATA);
        else
          CLEAR_RESULT;
        end if;
      end if;

      -- show error : may be set also if check error
      BLINK := FMT_ERROR; -- or CHK_ERROR

      if not IN_ACTION then

        if CON_IO."/="(NXT, CON_IO.TIMEOUT) then
          -- new field or error
          INS := FALSE;
          -- build field from data if not format error (new field)
          if not FMT_ERROR then
            TO_STR(CURR_FIELD, DATA, GET_STR, GET_LEN);
            POS := 1;
          end if;
        end if;

        -- get
        NAV_SCREEN.GET (CURR_FIELD, BLINK, GET_STR(1..GET_LEN), POS, INS, NXT);

        -- after validated get or timeout, clear error
        if CHK_ERROR or else FMT_ERROR then
          NAV_SCREEN.CLEAR_ERR;
        end if;
        CHK_ERROR := FALSE;
        FMT_ERROR := FALSE;

        -- if not timeout, try to have a value
        if CON_IO."/=" (NXT, CON_IO.TIMEOUT) then

          if CON_IO."=" (NXT, CON_IO.ESC) then
            -- on escape : unknown
            GET_STR (1) := '?';
          end if;

          -- Value of got field.
          TO_VALUE (CURR_FIELD, GET_STR(1..GET_LEN), DATA, OK, POS);

          if RESULT_PUT and then NAV_DATA."/=" (DATA, DATA_IN) then
            -- result (if any) is not valid any more
            CLEAR_RESULT;
            RESULT_PUT := FALSE;
          end if;


          if OK then
            -- put formated data in got field
            TO_STR(CURR_FIELD, DATA, GET_STR, GET_LEN);
            NAV_SCREEN.PUT (CURR_FIELD, GET_STR(1..GET_LEN), FALSE);
            -- Movement if ok
            case NXT is
              when CON_IO.UP | CON_IO.LEFT | CON_IO.STAB =>
                if NAV_DATA."/="
                 (CURR_FIELD, NAV_DATA.T_LIST_DATA'FIRST) then
                  CURR_FIELD := NAV_DATA.T_LIST_DATA'PRED(CURR_FIELD);
                else
                  IN_ACTION := TRUE;
                end if;
              when CON_IO.DOWN | CON_IO.RIGHT | CON_IO.TAB |
                   CON_IO.RET | CON_IO.FULL =>
                if NAV_DATA."/="
                 (CURR_FIELD, NAV_DATA.T_LIST_DATA'LAST) then
                  CURR_FIELD := NAV_DATA.T_LIST_DATA'SUCC(CURR_FIELD);
                else
                  IN_ACTION := TRUE;
                end if;
              when CON_IO.PGUP =>
                CURR_FIELD := NAV_DATA.T_LIST_DATA'FIRST;
              when CON_IO.PGDOWN =>
                IN_ACTION := TRUE;
              when CON_IO.CTRL_PGUP | CON_IO.CTRL_PGDOWN =>
                null;
              when CON_IO.REFRESH =>
                REFRESH := TRUE;
              when CON_IO.TIMEOUT | CON_IO.ESC |
                   CON_IO.MOUSE_BUTTON | CON_IO.BREAK =>
                -- impossible to be here
                null;
            end case;
            POS := 1;
          else
            -- format error and loop
            FMT_ERROR := TRUE;
            NAV_SCREEN.ERR_FORMAT;
          end if;
        end if;
      else -- in action

        -- get action
        CURR_ACTION := NAV_SCREEN.GET_ACTION;

        -- do action or movement
        case CURR_ACTION is
          when NAV_SCREEN.COMPUTE =>
            TO_DO := NAV_SCREEN.COMPUTE;
            return;
          when NAV_SCREEN.QUIT =>
            if NAV_SCREEN.CONFIRM_QUIT then
              TO_DO := NAV_SCREEN.QUIT;
              return;
            else
              REFRESH := TRUE;
            end if;
          when NAV_SCREEN.HELP =>
            NAV_SCREEN.PUT_HELP;
            REFRESH := TRUE;
          when NAV_SCREEN.CLEAR =>
            -- clear the data
            for FIELD in NAV_DATA.T_LIST_DATA loop
              DATA.SET (FIELD) := FALSE;
            end loop;
            REFRESH := TRUE;
            -- clear the result
            CLEAR_RESULT;
          when NAV_SCREEN.PREV =>
            IN_ACTION := FALSE;
            CURR_FIELD := NAV_DATA.T_LIST_DATA'LAST;
          when NAV_SCREEN.NEXT =>
            IN_ACTION := FALSE;
            CURR_FIELD := NAV_DATA.T_LIST_DATA'FIRST;
          when NAV_SCREEN.REFRESH =>
            REFRESH := TRUE;
        end case;

      end if;

    end loop;

  end GET;

  -- initialisation of screen and of remanent data
  procedure INIT is
  begin
    -- init screen
    CON_IO.INIT;
    NAV_SCREEN.RESET;

    -- get mask and data fields have to be put at next get
    REFRESH := TRUE;
    RESULT_PUT := FALSE;
    CHK_ERROR := FALSE;
  end INIT;

end NAV_DIALOG;


