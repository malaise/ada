with TEXT_IO;
with ARGUMENT, RND, SYS_CALLS, MY_MATH, INT_IO, FLO_IO, GET_FLOAT;
procedure RANDOM is

  NB_ARG : NATURAL;

  INT_FLOAT_MIN, INT_FLOAT_MAX : GET_FLOAT.INT_FLOAT_REC;

  ERROR : exception;

  package INTE_IO is new TEXT_IO.INTEGER_IO(MY_MATH.INTE);
  package REAL_IO is new TEXT_IO.FLOAT_IO(MY_MATH.REAL);

begin

  NB_ARG := ARGUMENT.GET_NBRE_ARG;

  if NB_ARG = 0 then
    -- No arg, 0 .. 1
    INT_FLOAT_MIN := (IS_FLOAT=> FALSE, INT_VALUE => 0);
    INT_FLOAT_MAX := (IS_FLOAT=> FALSE, INT_VALUE => 1);
  elsif NB_ARG = 1 then
    -- One arg, the max
    INT_FLOAT_MAX := GET_FLOAT.GET_INT_FLOAT(ARGUMENT.GET_PARAMETER(1));
    if INT_FLOAT_MAX.IS_FLOAT then
      INT_FLOAT_MIN := (IS_FLOAT=> TRUE, FLOAT_VALUE => 0.0);
    else
      INT_FLOAT_MIN := (IS_FLOAT=> FALSE, INT_VALUE => 0);
    end if;
  elsif NB_ARG = 2 then
    -- Two args, the min and max
    INT_FLOAT_MIN := GET_FLOAT.GET_INT_FLOAT(ARGUMENT.GET_PARAMETER(1));
    INT_FLOAT_MAX := GET_FLOAT.GET_INT_FLOAT(ARGUMENT.GET_PARAMETER(2));
    if INT_FLOAT_MIN.IS_FLOAT /= INT_FLOAT_MAX.IS_FLOAT then
      raise ERROR;
    end if;
  else
    raise ERROR;
  end if;

  RND.RANDOMIZE;
  if INT_FLOAT_MIN.IS_FLOAT then
    FLO_IO.PUT(RND.FLOAT_RANDOM(INT_FLOAT_MIN.FLOAT_VALUE, INT_FLOAT_MAX.FLOAT_VALUE));
  else
    INT_IO.PUT(RND.INT_RANDOM(INT_FLOAT_MIN.INT_VALUE, INT_FLOAT_MAX.INT_VALUE));
  end if;

exception
  when ERROR =>
    SYS_CALLS.PUT_LINE_ERROR ("ERROR. Usage: " & ARGUMENT.GET_PROGRAM_NAME & " [ [ <min> ] <max> ]");
    SYS_CALLS.PUT_LINE_ERROR (" min and max either both integers or both floats.");
    SYS_CALLS.PUT_LINE_ERROR (" default min is 0, default max is 1.");
end RANDOM;

