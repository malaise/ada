with COMMON, DATA;
package DISPLAY is

  procedure PRINT_TITTLE (KIND : in COMMON.COTE_KIND);

  -- The way for a cote
  type WAY_VECTOR is array (DATA.EFF_LINE_RANGE range <>)
                     of DATA.EFF_LINE_RANGE;

  procedure PRINT (KIND : in COMMON.COTE_KIND;
                   COTE : in DATA.EFF_COTE_RANGE;
                   WAY  : in WAY_VECTOR);

  procedure PUT_NO_WAY (KIND : in COMMON.COTE_KIND;
                        COTE : in DATA.EFF_COTE_RANGE);

  procedure PRINT_RESULT;

end DISPLAY;
