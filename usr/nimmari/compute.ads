with COMMON;
package COMPUTE is

  procedure INIT;

  type RESULT_LIST is (WON, LOST, PLAYED_AND_WON, PLAYED_AND_LOST, PLAYED);
  subtype PLAYED_RESULT_LIST is RESULT_LIST range PLAYED_AND_WON .. PLAYED;

  procedure PLAY (GAME : in COMMON.GAME_LIST;
                  RESULT : out RESULT_LIST;
                  ROW : out COMMON.ROW_RANGE;
                  BARS : out COMMON.FULL_BAR_RANGE);
end COMPUTE;
