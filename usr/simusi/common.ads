package COMMON is

  -- Max number of lines
  MAX_LINE : constant := 1_000;
  -- 2 lines -> 1 cote
  MAX_COTE : constant := MAX_LINE - 1;


  subtype LINE_RANGE is POSITIVE range 1 .. MAX_LINE;
  subtype COTE_RANGE is POSITIVE range 1 .. MAX_COTE;

  subtype POS_FLOAT is FLOAT RANGE 0.0 .. FLOAT'LAST;

  type COTE_KIND is (MANUFA, DESIGN);

  type COTE_REC (KIND : COTE_KIND) is record
    -- Lines of cote
    START, STOP : LINE_RANGE;
    -- Interval of cote
    INTER    : POS_FLOAT;
    case KIND is
      when MANUFA =>
        null;
      when DESIGN =>
        VALUE : POS_FLOAT;
    end case;
  end record;

  subtype MANUFA_COTE_REC is COTE_REC(MANUFA);
  subtype DESIGN_COTE_REC is COTE_REC(DESIGN);


  type MANUFA_ARRAY is array (COTE_RANGE range <>) of MANUFA_COTE_REC;
  type DESIGN_ARRAY is array (COTE_RANGE range <>) of DESIGN_COTE_REC;
end COMMON;
