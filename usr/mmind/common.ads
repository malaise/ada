package COMMON is

  ----------------
  -- DIMENSIONS --
  ----------------

  -- Maximum number of propal
  type PROPAL_RANGE is new POSITIVE range 1 .. 10;
  MAX_NUMBER_PROPAL : constant PROPAL_RANGE := PROPAL_RANGE'LAST;


  -- Minimum and maximum level
  type LEVEL_RANGE is new POSITIVE range 1 .. 5;
  subtype LAST_LEVEL_RANGE is LEVEL_RANGE range 3 .. LEVEL_RANGE'LAST;
  MIN_LEVEL : constant LAST_LEVEL_RANGE := LAST_LEVEL_RANGE'FIRST;
  MAX_LEVEL : constant LAST_LEVEL_RANGE := LAST_LEVEL_RANGE'LAST;


  -- Number of availble colors
  type COLOR_RANGE is new NATURAL range 0 .. 8;
  MAX_NUMBER_COLOR : constant COLOR_RANGE := COLOR_RANGE'LAST;
  subtype EFF_COLOR_RANGE is COLOR_RANGE range 1 .. MAX_NUMBER_COLOR;

  -- Level of the game
  procedure SET_LEVEL (LEVEL : in LAST_LEVEL_RANGE);
  function  GET_LEVEL return LAST_LEVEL_RANGE;

  -- Try state of a propal
  type TRY_LIST is (NOT_SET, CAN_TRY, ANSWERED);

  -- State of a propal
  type PROPAL_COLOR_ARRAY is array (LEVEL_RANGE range <>) of COLOR_RANGE;
  type PROPAL_STATE_REC (LEVEL : LAST_LEVEL_RANGE := MIN_LEVEL) is record
    PROPAL_COLOR : PROPAL_COLOR_ARRAY(1 .. LEVEL) := (others => 0);
    TRY          : TRY_LIST := NOT_SET;
  end record;

  function GET_PROPAL_STATE (PROPAL : PROPAL_RANGE) return PROPAL_STATE_REC;
  procedure SET_PROPAL_STATE (
   PROPAL : in PROPAL_RANGE;
   STATE  : in PROPAL_STATE_REC);

  procedure SET_COLOR (
   PROPAL : in PROPAL_RANGE;
   LEVEL  : in LEVEL_RANGE;
   COLOR  : in COLOR_RANGE);

  procedure SET_TRY_STATE (
   PROPAL : in PROPAL_RANGE;
   TRY    : in TRY_LIST);

  procedure SET_ANSWER (
   PROPAL : in PROPAL_RANGE;
   PLACED_OK, COLORS_OK : in NATURAL);

  procedure GET_ANSWER (
   PROPAL : in PROPAL_RANGE;
   PLACED_OK, COLORS_OK : out NATURAL);

  procedure RESET_STATE;



end COMMON;
