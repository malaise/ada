with RND;
package body RESPONSE is


  SECRET : COLOR_REC;

  function COLOR_RANDOM is new RND.DISCR_RANDOM (COMMON.EFF_COLOR_RANGE);

  procedure NEW_CODE is
    CURRENT_LEVEL : COMMON.LAST_LEVEL_RANGE := COMMON.GET_LEVEL;
  begin
    SECRET := (LEVEL => CURRENT_LEVEL,
              COLOR => (others => COMMON.EFF_COLOR_RANGE'FIRST) );

    for I in COMMON.LEVEL_RANGE
     range COMMON.LEVEL_RANGE'FIRST .. CURRENT_LEVEL loop
      SECRET.COLOR(I) := COLOR_RANDOM;
    end loop;
  end NEW_CODE;


  function RESPOND (PROPAL : COLOR_REC) return RESPONSE_REC is

    subtype COLUMN_RANGE is COMMON.LEVEL_RANGE range
     COMMON.LEVEL_RANGE'FIRST .. SECRET.LEVEL;
    SEEN_CODE, SEEN_PROPAL : array (COLUMN_RANGE) of BOOLEAN
                         := (others => FALSE);
    RESPONSE : RESPONSE_REC := (PLACED_OK => 0, COLORS_OK=> 0);
    use COMMON;
  begin
    if SECRET.LEVEL /= PROPAL.LEVEL then raise CONSTRAINT_ERROR; end if;

    for COL in COLUMN_RANGE loop
      if SECRET.COLOR(COL) = PROPAL.COLOR(COL) then
        RESPONSE.PLACED_OK := RESPONSE.PLACED_OK + 1;
        SEEN_CODE(COL) := TRUE;
        SEEN_PROPAL(COL) := TRUE;
      end if;
    end loop;

    for COL_PROPAL in COLUMN_RANGE loop
      if not SEEN_PROPAL (COL_PROPAL) then
        -- not a black
        for COL_CODE in COLUMN_RANGE loop
          if not SEEN_CODE(COL_CODE) and then
           SECRET.COLOR(COL_CODE) = PROPAL.COLOR(COL_PROPAL) then
            RESPONSE.COLORS_OK := RESPONSE.COLORS_OK + 1;
            SEEN_CODE(COL_CODE) := TRUE;
            SEEN_PROPAL(COL_PROPAL) := TRUE;
            exit;
          end if;
        end loop;
      end if;
    end loop;

    return RESPONSE;
  end RESPOND;



  function  GET_CODE return COLOR_REC is
  begin
    return SECRET;
  end GET_CODE;

end RESPONSE;
