with COMMON, SCREEN, COMPUTE;
procedure NIMMARI is
  GAME : COMMON.GAME_LIST;
  ROW : COMMON.ROW_RANGE;
  BARS : COMMON.FULL_BAR_RANGE;
  HUMAN, MACHINE : NATURAL := 0;
  RESULT : COMPUTE.RESULT_LIST;
  CHANGE_GAME : BOOLEAN;

  use COMMON, COMPUTE;
begin
  COMPUTE.INIT;
  GAME := SCREEN.INTRO;

  ONE_GAME:
  loop
    SCREEN.RESET (GAME);
    SCREEN.SCORE (HUMAN, MACHINE);
    ONE_GO:
    loop
      -- Compute game, check end
      COMPUTE.PLAY (GAME, RESULT, ROW, BARS);

      -- Update score
      if RESULT = COMPUTE.WON or else RESULT = COMPUTE.PLAYED_AND_WON then
        MACHINE := MACHINE + 1;
        SCREEN.SCORE (HUMAN, MACHINE);
      elsif RESULT = COMPUTE.LOST or else RESULT = COMPUTE.PLAYED_AND_LOST then
        HUMAN := HUMAN + 1;
        SCREEN.SCORE (HUMAN, MACHINE);
      end if;

      -- Display result
      SCREEN.UPDATE (ROW, BARS, RESULT, CHANGE_GAME);

      -- exit when end
      exit ONE_GO when RESULT /= COMPUTE.PLAYED;

      -- User plays
      SCREEN.PLAY;

    end loop ONE_GO;

    if CHANGE_GAME then
      if GAME = COMMON.NIM then
        GAME := COMMON.MARIENBAD;
      else
        GAME := COMMON.NIM;
      end if;
    end if;

  end loop ONE_GAME;

exception
  when SCREEN.EXIT_REQUESTED =>
    null;
end NIMMARI;

