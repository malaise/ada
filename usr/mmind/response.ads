with COMMON;
package RESPONSE is


  procedure NEW_CODE;


  type COLOR_ARRAY is array (COMMON.LEVEL_RANGE range <>) of
   COMMON.EFF_COLOR_RANGE;
  type COLOR_REC (LEVEL : COMMON.LAST_LEVEL_RANGE := COMMON.MIN_LEVEL)
   is record
    COLOR : COLOR_ARRAY (1 .. LEVEL);
  end record;

  function GET_CODE return COLOR_REC;

  type RESPONSE_REC is record
    PLACED_OK : NATURAL;
    COLORS_OK  : NATURAL;
  end record;

  function RESPOND (PROPAL : COLOR_REC) return RESPONSE_REC;

end RESPONSE;