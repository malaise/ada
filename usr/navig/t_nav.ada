with NAV_TYPES, NAV_DATA;
with MY_IO, TEXT_IO;
procedure T_NAV is
str : string (1..20);
lst : natural;

  DATA_IN, DATA_OUT : NAV_DATA.T_DATA;
  subtype TI is POSITIVE range 1..7;
  I1, I2, I3 : TI;
  REPORT : NAV_DATA.T_CONSISTENCY;
  package CONS_IO is new TEXT_IO.ENUMERATION_IO (NAV_DATA.T_CONSISTENCY);

  function DOT (S : STRING) return NATURAL is
  begin
    for I in S'RANGE loop
      if S(I)='.' then return I; end if;
    end loop;
    return 0;
  end DOT;

  function GET_INDICE (MSG : STRING) return TI is
    S : STRING (1..10);
    L : NATURAL;
  begin
    loop
      begin
        MY_IO.PUT (MSG & " unknown ? ");
        MY_IO.GET_LINE (S, L);
        return TI'VALUE (S(1..L));
      exception
        when others => MY_IO.PUT_LINE ("ERREUR...");
      end;
    end loop;
  end GET_INDICE;

  function GET_ANGLE (MSG : STRING) return NAV_TYPES.T_ANGLE is
    S : STRING (1..10);
    L : NATURAL;
    D : NATURAL;
    A : NAV_TYPES.T_ANGLE;
  begin
    loop
      begin
        MY_IO.PUT (MSG & " angle ? ");
        MY_IO.GET_LINE (S, L);
        D := DOT (S(1..L));
        if D = 0 then
          A.DEGREES := NAV_TYPES.T_DEGREE'VALUE (S(1..L));
          A.MINUTES := 0;
        else
          A.DEGREES := NAV_TYPES.T_DEGREE'VALUE (S(1..D-1));
          A.MINUTES := NAV_TYPES.T_MINUTE'VALUE (S(D+1..L));
        end if;
        return A;
      exception
        when others => MY_IO.PUT_LINE ("ERREUR...");
      end;
    end loop;
  end GET_ANGLE;

  function GET_DRIFT (MSG : STRING) return NAV_TYPES.T_DRIFT is
    S : STRING (1..10);
    L : NATURAL;
    D : NATURAL;
    A : NAV_TYPES.T_DRIFT;
  begin
    loop
      begin
        MY_IO.PUT (MSG & " angle ? ");
        MY_IO.GET_LINE (S, L);
        if S(1)='-'  then
          S(1..L-1) := S(2..L);
          L := L - 1;
          A.POSITIV := FALSE;
        else
          A.POSITIV := TRUE;
        end if;
        D := DOT (S(1..L));
        if D = 0 then
          A.DEGREES := NAV_TYPES.T_DEG_DRIFT'VALUE (S(1..L));
          A.MINUTES := 0;
        else
          A.DEGREES := NAV_TYPES.T_DEG_DRIFT'VALUE (S(1..D-1));
          A.MINUTES := NAV_TYPES.T_MINUTE'VALUE (S(D+1..L));
        end if;
        return A;
      exception
        when others => MY_IO.PUT_LINE ("ERREUR...");
      end;
    end loop;
  end GET_DRIFT;

  function GET_SPEED (MSG : STRING) return NAV_TYPES.T_SPEED is
    S : STRING (1..10);
    L : NATURAL;
  begin
    loop
      begin
        MY_IO.PUT (MSG & " speed ? ");
        MY_IO.GET_LINE (S, L);
        return NAV_TYPES.T_SPEED (POSITIVE'VALUE (S(1..L)));
      exception
        when others => MY_IO.PUT_LINE ("ERREUR...");
      end;
    end loop;
  end GET_SPEED;

begin
  MY_IO.PUT_LINE ("1 : WIND SPEED");
  MY_IO.PUT_LINE ("2 : WIND ANGLE");
  MY_IO.PUT_LINE ("3 : PLAN SPEED");
  MY_IO.PUT_LINE ("4 : PLAN ANGLE");
  MY_IO.PUT_LINE ("5 : TRAJ SPEED");
  MY_IO.PUT_LINE ("6 : TRAJ ANGLE");
  MY_IO.PUT_LINE ("7 : DRIFT");
  I1 := GET_INDICE ("1st");
  loop
    I2 := GET_INDICE ("2nd");
    exit when I2 /= I1;
  end loop;
  loop
    I3 := GET_INDICE ("3rd");
    exit when I3 /= I1 and then I3 /= I2;
  end loop;
  DATA_IN.SET(NAV_DATA.WIND_S) := I1 /= 1 and then I2 /= 1 and then I3 /= 1;
  DATA_IN.SET(NAV_DATA.WIND_A) := I1 /= 2 and then I2 /= 2 and then I3 /= 2;
  DATA_IN.SET(NAV_DATA.PLAN_S) := I1 /= 3 and then I2 /= 3 and then I3 /= 3;
  DATA_IN.SET(NAV_DATA.PLAN_A) := I1 /= 4 and then I2 /= 4 and then I3 /= 4;
  DATA_IN.SET(NAV_DATA.TRAJ_S) := I1 /= 5 and then I2 /= 5 and then I3 /= 5;
  DATA_IN.SET(NAV_DATA.TRAJ_A) := I1 /= 6 and then I2 /= 6 and then I3 /= 6;
  DATA_IN.SET(NAV_DATA.DRIFT)  := I1 /= 7 and then I2 /= 7 and then I3 /= 7;

  if DATA_IN.SET (NAV_DATA.WIND_S) then
    DATA_IN.WIND.SPEED := GET_SPEED("wind");
  end if;
  if DATA_IN.SET (NAV_DATA.WIND_A) then
    DATA_IN.WIND.ANGLE := GET_ANGLE ("wind");
  end if;

  if DATA_IN.SET (NAV_DATA.PLAN_S) then
    DATA_IN.PLAN.SPEED := GET_SPEED("plan");
  end if;
  if DATA_IN.SET (NAV_DATA.PLAN_A) then
    DATA_IN.PLAN.ANGLE := GET_ANGLE ("plan");
  end if;

  if DATA_IN.SET (NAV_DATA.TRAJ_S) then
    DATA_IN.TRAJ.SPEED := GET_SPEED("traj");
  end if;
  if DATA_IN.SET (NAV_DATA.TRAJ_A) then
    DATA_IN.TRAJ.ANGLE := GET_ANGLE ("traj");
  end if;


  if DATA_IN.SET (NAV_DATA.DRIFT) then
    DATA_IN.DRIFT := GET_DRIFT ("drift");
  end if;

  NAV_DATA.RESOLUTION (DATA_IN, REPORT, DATA_OUT);
  MY_IO.NEW_LINE;
  CONS_IO.PUT (REPORT);
  MY_IO.NEW_LINE;
  if NAV_DATA."/=" (REPORT, NAV_DATA.OK) then return; end if;

  MY_IO.PUT ("WIND SPEED "); MY_IO.PUT_LINE (DATA_OUT.WIND.SPEED);
  MY_IO.PUT ("WIND ANGLE "); MY_IO.PUT (INTEGER(DATA_OUT.WIND.ANGLE.DEGREES));
  MY_IO.PUT ("."); MY_IO.PUT_LINE (INTEGER(DATA_OUT.WIND.ANGLE.MINUTES));

  MY_IO.PUT ("PLAN SPEED "); MY_IO.PUT_LINE (DATA_OUT.PLAN.SPEED);
  MY_IO.PUT ("PLAN ANGLE "); MY_IO.PUT (INTEGER(DATA_OUT.PLAN.ANGLE.DEGREES));
  MY_IO.PUT ("."); MY_IO.PUT_LINE (INTEGER(DATA_OUT.PLAN.ANGLE.MINUTES));

  MY_IO.PUT ("TRAJ SPEED "); MY_IO.PUT_LINE (DATA_OUT.TRAJ.SPEED);
  MY_IO.PUT ("TRAJ ANGLE "); MY_IO.PUT (INTEGER(DATA_OUT.TRAJ.ANGLE.DEGREES));
  MY_IO.PUT ("."); MY_IO.PUT_LINE (INTEGER(DATA_OUT.TRAJ.ANGLE.MINUTES));

  MY_IO.PUT ("DRIFT ");
  if DATA_OUT.DRIFT.POSITIV then
    MY_IO.PUT ("+");
  else
    MY_IO.PUT ("-");
  end if;
  MY_IO.PUT (INTEGER(DATA_OUT.DRIFT.DEGREES)); MY_IO.PUT (".");
  MY_IO.PUT_LINE (INTEGER(DATA_OUT.DRIFT.MINUTES));
end T_NAV;

