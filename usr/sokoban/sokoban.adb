with MY_IO;
with ARGUMENT;
with RND;
with UPPER_STR;
with SOK_TYPES;
with SOK_MANAGER;

procedure SOKOBAN is
  NO_FRAME : SOK_TYPES.FRAME_RANGE;
  OK : BOOLEAN := FALSE;

  function FRAME_RANDOM is new RND.DISCR_RANDOM(SOK_TYPES.FRAME_RANGE);

  procedure USAGE is
  begin
    MY_IO.PUT_LINE ("Usage : SOKOBAN [frame_number]");
    MY_IO.PUT_LINE (" Frames are from 1 to 50");
  end USAGE;

begin
  RND.RANDOMIZE;

  begin
    if ARGUMENT.GET_NBRE_ARG = 0 then
      NO_FRAME := SOK_TYPES.FRAME_RANGE'FIRST;
    elsif ARGUMENT.GET_NBRE_ARG = 1 then
      if UPPER_STR (ARGUMENT.GET_PARAMETER) = "RND" then
        NO_FRAME := FRAME_RANDOM;
      else
        NO_FRAME := SOK_TYPES.FRAME_RANGE'VALUE (ARGUMENT.GET_PARAMETER);
      end if;
    else
      raise CONSTRAINT_ERROR;
    end if;
    OK := TRUE;
  exception
    when others =>
      USAGE;
      OK := FALSE;
  end;
  if OK then
    SOK_MANAGER.PLAY_GAME(FIRST_FRAME => NO_FRAME);
  end if;
exception
  when others =>
    delay 5.0;
    raise;
end SOKOBAN;
