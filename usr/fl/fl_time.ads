package FL_TIME is

  type HOURS_RANGE   is range 0 .. 999_999_999;
  type MINUTES_RANGE is range 0 .. 59;

  type TIME_TYPE is record
    POSITIV : BOOLEAN       := TRUE;
    HOURS   : HOURS_RANGE   := 0;
    MINUTES : MINUTES_RANGE := 0;
  end record;

  function "+" (LEFT, RIGHT : TIME_TYPE) return TIME_TYPE;

  function "-" (LEFT, RIGHT : TIME_TYPE) return TIME_TYPE;

  TIME_OVERFLOW : exception;

end FL_TIME;
