package body COMMON is

  type FIX_PROPAL_STATE_REC is record
    PROPAL_COLOR : PROPAL_COLOR_ARRAY(1 .. MAX_LEVEL) := (others => 0);
    TRY          : TRY_LIST := NOT_SET;
    PLACED_OK    : NATURAL := 0;
    COLORS_OK    : NATURAL := 0;
  end record;
  INIT_FIX_PROPAL_STATE : constant FIX_PROPAL_STATE_REC := (
    PROPAL_COLOR => (others => 0),
    TRY          => NOT_SET,
    PLACED_OK    => 0,
    COLORS_OK    => 0);

  STATE_LEVEL_STORED : LAST_LEVEL_RANGE;
  STATE_LEVEL : LAST_LEVEL_RANGE;
  LEVEL_STORED : BOOLEAN := FALSE;
  LEVEL_SET   : BOOLEAN := FALSE;
  STATE_DATA  : array (PROPAL_RANGE) of FIX_PROPAL_STATE_REC;

  -- Level of the game
  --  Store the one selected (may not be the one of current propal)
  procedure STORE_LEVEL (LEVEL : in LAST_LEVEL_RANGE) is
  begin
    STATE_LEVEL_STORED := LEVEL;
    LEVEL_STORED := TRUE;
  end STORE_LEVEL;

  --  Set propal to level stored
  procedure SET_LEVEL_TO_STORED is
  begin
    if not LEVEL_STORED then raise CONSTRAINT_ERROR; end if;
    STATE_LEVEL := STATE_LEVEL_STORED;
    LEVEL_SET := TRUE;
    RESET_STATE;
  end SET_LEVEL_TO_STORED;

  procedure CHECK_LEVEL is
  begin
    if not LEVEL_SET then raise CONSTRAINT_ERROR; end if;
  end CHECK_LEVEL;

  function GET_STORED_LEVEL return LAST_LEVEL_RANGE is
  begin
    if not LEVEL_STORED then raise CONSTRAINT_ERROR; end if;
    return STATE_LEVEL_STORED;
  end GET_STORED_LEVEL;

  function GET_LEVEL return LAST_LEVEL_RANGE is
  begin
    CHECK_LEVEL;
    return STATE_LEVEL;
  end GET_LEVEL;

  function GET_PROPAL_STATE (PROPAL : PROPAL_RANGE) return PROPAL_STATE_REC is
  begin
    CHECK_LEVEL;
    return (LEVEL        => STATE_LEVEL,
            PROPAL_COLOR => STATE_DATA(PROPAL).PROPAL_COLOR(1 .. STATE_LEVEL),
            TRY          => STATE_DATA(PROPAL).TRY );
  end GET_PROPAL_STATE;

  procedure SET_PROPAL_STATE (
   PROPAL : in PROPAL_RANGE;
   STATE  : in PROPAL_STATE_REC) is
  begin
    CHECK_LEVEL;
    if STATE.LEVEL /= STATE_LEVEL then raise CONSTRAINT_ERROR; end if;
    STATE_DATA(PROPAL).PROPAL_COLOR(1 .. STATE_LEVEL) := STATE.PROPAL_COLOR;
    STATE_DATA(PROPAL).TRY := STATE.TRY;
  end SET_PROPAL_STATE;

  procedure SET_COLOR (
   PROPAL : in PROPAL_RANGE;
   LEVEL  : in LEVEL_RANGE;
   COLOR  : in COLOR_RANGE) is
  begin
    CHECK_LEVEL;
    STATE_DATA(PROPAL).PROPAL_COLOR(LEVEL) := COLOR;
  end SET_COLOR;

  procedure SET_TRY_STATE (
   PROPAL : in PROPAL_RANGE;
   TRY    : in TRY_LIST) is
  begin
    CHECK_LEVEL;
    STATE_DATA(PROPAL).TRY := TRY;
  end SET_TRY_STATE;

  procedure SET_ANSWER (
   PROPAL : in PROPAL_RANGE;
   PLACED_OK, COLORS_OK : in NATURAL) is
  begin
    CHECK_LEVEL;
    if STATE_DATA(PROPAL).TRY /= ANSWERED then
      raise CONSTRAINT_ERROR;
    end if;
    STATE_DATA(PROPAL).PLACED_OK := PLACED_OK;
    STATE_DATA(PROPAL).COLORS_OK := COLORS_OK;
  end SET_ANSWER;

  procedure GET_ANSWER (
   PROPAL : in PROPAL_RANGE;
   PLACED_OK, COLORS_OK : out NATURAL) is
  begin
    CHECK_LEVEL;
    if STATE_DATA(PROPAL).TRY /= ANSWERED then
      raise CONSTRAINT_ERROR;
    end if;
    PLACED_OK := STATE_DATA(PROPAL).PLACED_OK;
    COLORS_OK := STATE_DATA(PROPAL).COLORS_OK;
  end GET_ANSWER;

  procedure RESET_STATE is
  begin
    CHECK_LEVEL;
    STATE_DATA := (others => INIT_FIX_PROPAL_STATE);
  end RESET_STATE;

end COMMON;
