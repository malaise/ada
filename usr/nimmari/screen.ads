with COMMON, COMPUTE;

package SCREEN is

  EXIT_REQUESTED : exception;

  function INTRO return COMMON.GAME_LIST;


  procedure RESET (GAME : in COMMON.GAME_LIST);

  procedure PLAY;

  function CONTENT (ROW : COMMON.ROW_RANGE) return COMMON.FULL_BAR_RANGE;


  procedure UPDATE (ROW : in COMMON.ROW_RANGE; BARS : in COMMON.FULL_BAR_RANGE;
                    RESULT : in COMPUTE.RESULT_LIST; CHANGE_GAME : out BOOLEAN);

  procedure SCORE (HUMAN, MACHINE : in NATURAL);

end SCREEN;
