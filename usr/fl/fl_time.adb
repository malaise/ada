package body FL_TIME is

  MAX_MIN : constant INTEGER := INTEGER (MINUTES_RANGE'LAST) + 1;

  function ABS_TIME (VAL : TIME_TYPE) return TIME_TYPE is
  begin
    return (POSITIV => TRUE, HOURS => VAL.HOURS, MINUTES => VAL.MINUTES);
  end ABS_TIME;

  function "-" (VAL : TIME_TYPE) return TIME_TYPE is
    RESULT : TIME_TYPE;
  begin
    return (POSITIV => not VAL.POSITIV,
            HOURS   => VAL.HOURS,
            MINUTES => VAL.MINUTES);
  end "-";

  function "<" (LEFT, RIGHT : TIME_TYPE) return BOOLEAN is
  begin
    if LEFT.POSITIV = RIGHT.POSITIV then
      -- Same sign
      if LEFT.POSITIV then
        return
         LEFT.HOURS < RIGHT.HOURS or else
         (LEFT.HOURS = RIGHT.HOURS and then LEFT.MINUTES < RIGHT.MINUTES);
      else
        return
         LEFT.HOURS > RIGHT.HOURS or else
         (LEFT.HOURS = RIGHT.HOURS and then LEFT.MINUTES > RIGHT.MINUTES);
      end if;
    else
      return RIGHT.POSITIV;
    end if;
  end "<";

  function "+" (LEFT, RIGHT : TIME_TYPE) return TIME_TYPE is
    RESULT : TIME_TYPE;
  begin
    if LEFT.POSITIV = RIGHT.POSITIV then
      RESULT.MINUTES := MINUTES_RANGE (
       (INTEGER (LEFT.MINUTES) + INTEGER (RIGHT.MINUTES)) mod MAX_MIN);
      RESULT.HOURS := HOURS_RANGE (
       (INTEGER (LEFT.MINUTES) + INTEGER (RIGHT.MINUTES)) / MAX_MIN);
      RESULT.HOURS := RESULT.HOURS + LEFT.HOURS + RIGHT.HOURS;
      RESULT.POSITIV := LEFT.POSITIV;
      return RESULT;
    elsif LEFT.POSITIV then
      return LEFT - ABS_TIME(RIGHT);
    else
      return - (ABS_TIME(LEFT) - RIGHT);
    end if;
  exception
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      raise TIME_OVERFLOW;
  end "+";

  function "-" (LEFT, RIGHT : TIME_TYPE) return TIME_TYPE is
    RESULT : TIME_TYPE;
  begin
    if LEFT.POSITIV = RIGHT.POSITIV then
      if not (ABS_TIME(LEFT) < ABS_TIME(RIGHT)) then

        if LEFT.MINUTES >= RIGHT.MINUTES then
          RESULT.MINUTES := LEFT.MINUTES - RIGHT.MINUTES;
          RESULT.HOURS := LEFT.HOURS - RIGHT.HOURS;
        else
          RESULT.MINUTES := MINUTES_RANGE (
             MAX_MIN
           + INTEGER (LEFT.MINUTES)
           - INTEGER (RIGHT.MINUTES));
          RESULT.HOURS := LEFT.HOURS - RIGHT.HOURS - 1;
        end if;
        RESULT.POSITIV := LEFT.POSITIV;
        return RESULT;
      else
        return - (RIGHT - LEFT);
      end if;
    else
      return LEFT + (- RIGHT);
    end if;
  exception
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      raise TIME_OVERFLOW;
  end "-";

end FL_TIME;
