with SORTS;
package body POINTS is

  subtype T_RANGE is POSITIVE range 1 .. MAX_NUMBER;

  -- Points storage
  type T_STORAGE is
    record
      NUMBER     : NATURAL range 0 .. MAX_NUMBER := 0;
      THE_POINTS : P_T_THE_POINTS(T_RANGE);
      SAVED      : BOOLEAN := TRUE;
    end record;
  STORAGE : T_STORAGE;

  function LT (PL, PR : in P_T_ONE_POINT) return BOOLEAN is
  begin
    return PL.X < PR.X or else
         (PL.X = PR.X and then PL.Y < PR.Y);
  end LT;

  package POINTS_SORT is new SORTS (
    TYP_OBJECT => P_T_ONE_POINT,
    TYP_INDEX  => POSITIVE,
    "<"        => LT,
    TYP_ARRAY  => P_T_THE_POINTS);

  function P_THE_POINTS return P_T_THE_POINTS is
  begin
    return STORAGE.THE_POINTS(1 .. STORAGE.NUMBER);
  end P_THE_POINTS;

  function P_ONE_POINT (INDEX : in POSITIVE) return P_T_ONE_POINT is
  begin
    if INDEX < 1 or else INDEX > STORAGE.NUMBER then
      raise P_INDEX_OUT;
    end if;
    return STORAGE.THE_POINTS(INDEX);
  end P_ONE_POINT;

  -- Store points
  procedure P_STORE(THE_POINTS : in P_T_THE_POINTS) is
  begin
    -- Raise constraint error if the number of points is too big
    STORAGE.NUMBER := THE_POINTS'LENGTH;
    if STORAGE.NUMBER > 0 then
      STORAGE.THE_POINTS(1 .. STORAGE.NUMBER) :=
       THE_POINTS(THE_POINTS'RANGE);
    end if;
    STORAGE.SAVED := TRUE;
  exception
    when CONSTRAINT_ERROR =>
      raise P_TOO_MANY;
  end P_STORE;

  -- Clear storage
  procedure P_CLEAR is
  begin
    STORAGE.NUMBER := 0;
    STORAGE.SAVED := TRUE;
  end P_CLEAR;

  -- Sort
  procedure P_SORT is
  begin
    POINTS_SORT.QUICK_SORT(STORAGE.THE_POINTS(1 .. STORAGE.NUMBER));
  end P_SORT;
  

  -- Take a point update into account
  procedure P_UPD_POINT(ACTION : in P_T_UPD_ACTION;
                        INDEX  : in POSITIVE := 1;
                        POINT  : in P_T_ONE_POINT := (X => 0.0, Y => 0.0)) is
  begin
    case ACTION is
      when ADD =>
        -- Append new point
        begin
          STORAGE.NUMBER := STORAGE.NUMBER + 1;
        exception
          when CONSTRAINT_ERROR =>
            raise P_TOO_MANY;
        end;
        STORAGE.THE_POINTS(STORAGE.NUMBER) := POINT;
      when REMOVE =>
        if INDEX < 1 or else INDEX > STORAGE.NUMBER then
          raise P_INDEX_OUT;
        end if;
        -- Shift points
        STORAGE.NUMBER := STORAGE.NUMBER - 1;
        STORAGE.THE_POINTS(INDEX .. STORAGE.NUMBER) :=
          STORAGE.THE_POINTS(INDEX + 1 .. STORAGE.NUMBER + 1);
      when MODIFY =>
        -- Store new content
        if INDEX < 1 or else INDEX > STORAGE.NUMBER then
          raise P_INDEX_OUT;
        end if;
        STORAGE.THE_POINTS(INDEX) := POINT;
    end case;
    -- The new set is not saved
    STORAGE.SAVED := FALSE;
  end P_UPD_POINT;

  procedure P_SAVED is
  begin
    STORAGE.SAVED := TRUE;
  end P_SAVED;

  function P_SAVED return BOOLEAN is
  begin
    return STORAGE.SAVED;
  end P_SAVED;

  function P_EMPTY return BOOLEAN is
  begin
    return STORAGE.NUMBER = 0;
  end P_EMPTY;

  function P_NB return NATURAL is
  begin
    return STORAGE.NUMBER;
  end P_NB;

end POINTS;
