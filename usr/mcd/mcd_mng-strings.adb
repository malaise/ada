with TEXT_HANDLER, UPPER_STR, LOWER_STR;
separate(MCD_MNG)

package body STRINGS is

  procedure CHECK_CHRS (X : in ITEM_REC) is
  begin
    if X.KIND /= CHRS then
      raise INVALID_ARGUMENT;
    end if;
  end CHECK_CHRS;

  procedure CHECK_INTE (X : in ITEM_REC) is
  begin
    if X.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
  end CHECK_INTE;

  function STRCAT (S1, S2 : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);
  begin
    CHECK_CHRS(S1);
    CHECK_CHRS(S2);

    if S1.VAL_LEN + S2.VAL_LEN > CHARS_TEXT'LENGTH then
      raise STRING_LEN;
    end if;
    RES.VAL_LEN := S1.VAL_LEN + S2.VAL_LEN;
    RES.VAL_TEXT(1 .. RES.VAL_LEN) := S1.VAL_TEXT(1 .. S1.VAL_LEN)
                                    & S2.VAL_TEXT(1 .. S2.VAL_LEN);
    return RES;
  end STRCAT;

  function STRSUB (S, I1, I2 : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);
  begin
    CHECK_CHRS(S);
    CHECK_INTE(I1);
    CHECK_INTE(I2);

    -- Empty string
    if I2.VAL_INTE = 0 or else I2.VAL_INTE < I1.VAL_INTE then
      RES.VAL_LEN := 0;
      return RES;
    end if;


    if I1.VAL_INTE < 1 or else I1.VAL_INTE > MY_MATH.INTE(S.VAL_LEN) then
      raise ARGUMENT_MISMATCH;
    end if;
    if I2.VAL_INTE < 1 or else I2.VAL_INTE > MY_MATH.INTE(S.VAL_LEN) then
      raise ARGUMENT_MISMATCH;
    end if;
    RES.VAL_LEN := NATURAL(I2.VAL_INTE) - NATURAL(I1.VAL_INTE) + 1;
    RES.VAL_TEXT(1 .. RES.VAL_LEN) :=
            S.VAL_TEXT(NATURAL(I1.VAL_INTE) .. NATURAL(I2.VAL_INTE));
    return RES;

  end STRSUB;

  function STRLOC (OCC, PAT, S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(INTE);
  begin
    CHECK_CHRS(S);
    CHECK_CHRS(PAT);
    CHECK_INTE(OCC);

    if OCC.VAL_INTE < 1 or else OCC.VAL_INTE > MY_MATH.INTE(POSITIVE'LAST) then
      raise ARGUMENT_MISMATCH;
    end if;

    declare
      TXT : TEXT_HANDLER.TEXT(TEXT_HANDLER.MAX_LEN_RANGE(S.VAL_LEN));
    begin
      TEXT_HANDLER.SET(TXT, S.VAL_TEXT(1 .. S.VAL_LEN));
      RES.VAL_INTE := MY_MATH.INTE(
          TEXT_HANDLER.LOCATE(WITHIN    => TXT,
                              FRAGMENT  => PAT.VAL_TEXT(1 .. PAT.VAL_LEN),
                              OCCURENCE => POSITIVE(OCC.VAL_INTE)) );
    end;
    return RES;
  end STRLOC;

  function STRREP (I, PAT, S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);
  begin
    CHECK_CHRS(S);
    CHECK_CHRS(PAT);
    CHECK_INTE(I);

    if I.VAL_INTE < 1 or else I.VAL_INTE > MY_MATH.INTE(S.VAL_LEN) then
      raise ARGUMENT_MISMATCH;
    end if;

    declare
      TXT : TEXT_HANDLER.TEXT(CHARS_TEXT'LENGTH);
    begin
      TEXT_HANDLER.SET(TXT, S.VAL_TEXT(1 .. S.VAL_LEN));
      TEXT_HANDLER.AMEND(TO => TXT, 
                         BY => PAT.VAL_TEXT(1 .. PAT.VAL_LEN),
                         POSITION => TEXT_HANDLER.MAX_LEN_RANGE(I.VAL_INTE) );
      if TEXT_HANDLER.LENGTH(TXT) > CHARS_TEXT'LENGTH then
        raise STRING_LEN;
      end if;
      RES.VAL_LEN := TEXT_HANDLER.LENGTH(TXT);
      RES.VAL_TEXT(1 .. RES.VAL_LEN) := TEXT_HANDLER.VALUE(TXT);
    end;
    return RES;
  end STRREP;

  function STRLEN (S : ITEM_REC) return ITEM_REC is
  begin
    CHECK_CHRS(S);
    return (KIND => INTE, VAL_INTE => MY_MATH.INTE(S.VAL_LEN));
  end STRLEN;

  function STRUPP (S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);
  begin
    CHECK_CHRS(S);
    RES.VAL_LEN := S.VAL_LEN;
    RES.VAL_TEXT(1 .. RES.VAL_LEN) :=
      UPPER_STR(S.VAL_TEXT(1 .. S.VAL_LEN));
    return RES;
  end STRUPP;

  function STRLOW (S : ITEM_REC) return ITEM_REC is
    RES : ITEM_REC(CHRS);
  begin
    CHECK_CHRS(S);
    RES.VAL_LEN := S.VAL_LEN;
    RES.VAL_TEXT(1 .. RES.VAL_LEN) :=
      LOWER_STR(S.VAL_TEXT(1 .. S.VAL_LEN));
    return RES;
  end STRLOW;

end STRINGS;

