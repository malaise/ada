with CON_IO;
with COMMON;
package SCREEN is

  -------------------
  -- GLOBAL SCREEN --
  -------------------
  -- init the screen
  procedure INIT (LEVEL  : in COMMON.LAST_LEVEL_RANGE);

  -- clear and reset
  procedure CLEAR;

  ------------
  -- PROPAL --
  ------------
  procedure PUT_DEFAULT_POS (
   PROPAL : in COMMON.PROPAL_RANGE;
   LEVEL  : in COMMON.LEVEL_RANGE;
   SHOW   : in BOOLEAN);

  type PUT_TRY_LIST is (CANNOT_TRY, CAN_TRY, SELECTED);
  procedure PUT_TRY (
   PROPAL    : in COMMON.PROPAL_RANGE;
   TRY_STATE : in PUT_TRY_LIST);

  procedure PUT_COLOR (
   PROPAL : in COMMON.PROPAL_RANGE;
   LEVEL  : in COMMON.LEVEL_RANGE;
   COLOR  : in COMMON.COLOR_RANGE);

  procedure PUT_ANSWER (
   PROPAL : in COMMON.PROPAL_RANGE;
   PLACED_OK, COLORS_OK : in NATURAL);


  ------------
  -- SECRET --
  ------------
  procedure PUT_SECRET_COLOR (
   LEVEL  : in COMMON.LEVEL_RANGE;
   COLOR  : in COMMON.COLOR_RANGE);

  ----------
  -- MENU --
  ----------
  procedure PUT_START_GIVEUP (START : in BOOLEAN; SELECTED : in BOOLEAN);

  procedure PUT_EXIT (SELECTED : in BOOLEAN);

  -----------
  -- LEVEL --
  -----------
  procedure PUT_LEVEL (LEVEL_NO : in COMMON.LAST_LEVEL_RANGE;
   SELECTED : in BOOLEAN);

  procedure PUT_CURRENT_LEVEL (LEVEL_NO : in COMMON.LAST_LEVEL_RANGE);


  -----------
  -- COLOR --
  -----------
  procedure PUT_SELECTED_COLOR (
   COLOR    : in COMMON.EFF_COLOR_RANGE;
   SELECTED : in BOOLEAN);

  ----------
  -- HELP --
  ----------
  type HELP_STATE is (RELEASED, CLICK_COLOR, CLICK_PROPAL, CLICK_OTHER,
                      START, DISCARDED);
  procedure PUT_HELP (HELP : HELP_STATE);


  -----------
  -- MOUSE --
  -----------
  -- default behaviour of mouse : keep foreground
  procedure SET_MOUSE_DEFAULT_COLOR;
  -- When color selected : set foreground
  procedure SET_MOUSE_COLOR (COLOR : in COMMON.EFF_COLOR_RANGE);


  ---------------
  -- SELECTION --
  ---------------
  -- kind of selection
  type SELECTION_LIST is (NOTHING, COLOR, PROPAL, TRY, MENU, LEVEL, EXIT_GAME);


  -- Selection data
  type SELECTION_REC(SELECTION_KIND : SELECTION_LIST := NOTHING) is record
    case SELECTION_KIND is
      when NOTHING =>
        SELECTION : SELECTION_LIST := NOTHING;
      when COLOR =>
        COLOR_NO : COMMON.EFF_COLOR_RANGE;
      when PROPAL =>
        PROPAL_NO : COMMON.PROPAL_RANGE;
        COLUMN_NO : COMMON.LEVEL_RANGE;
      when TRY=>
        TRY_NO : COMMON.PROPAL_RANGE;
      when MENU | EXIT_GAME =>
        null;
      WHEN LEVEL =>
        LEVEL_NO : COMMON.LAST_LEVEL_RANGE;
    end case;
  end record;


  procedure GET_SELECTED (
   WHERE : in CON_IO.SQUARE;
   WHAT  : out SELECTION_REC);

 end SCREEN;
