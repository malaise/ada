with OPER_DEF;
package UNIT_FORMAT is

  type UNITS_LIST is (EUROS, FRANCS);
  DEFAULT_UNIT : constant UNITS_LIST := EUROS;

  -- Current unit switching
  function GET_CURRENT_UNIT return UNITS_LIST;
  procedure SET_UNIT_TO (UNIT : UNITS_LIST);

  --  May be raised by any IMAGE/VALUE
  FORMAT_ERROR : EXCEPTION;

  -- Date: 25/10/2001
  subtype DATE_STR is STRING(1 .. 10);
  function DATE_IMAGE(DATE : OPER_DEF.DATE_REC) return DATE_STR;
  function DATE_VALUE(STR : DATE_STR) return OPER_DEF.DATE_REC;

  -- Short date: 25/10/01
  subtype SHORT_DATE_STR is STRING(1 .. 8);
  function SHORT_DATE_IMAGE(DATE : OPER_DEF.DATE_REC) return SHORT_DATE_STR;

  -- Short status: Yes No Def
  subtype SHORT_STATUS_STR is STRING(1 .. 3);
  function SHORT_STATUS_IMAGE (STATUS : OPER_DEF.STATUS_LIST)
           return SHORT_STATUS_STR;

  -- Short kind:  Cheq Card Tran Draw
  subtype SHORT_KIND_STR is STRING(1 .. 4);
  function SHORT_KIND_IMAGE (KIND : OPER_DEF.KIND_LIST)
           return SHORT_KIND_STR;

  -- Amount: -12345678.12
  subtype AMOUNT_STR is STRING (1 .. 12);
  -- From an amount (in euros) return 'image (euros/francs)
  function IMAGE (AMOUNT_IN_EUROS : OPER_DEF.AMOUNT_RANGE)
                 return AMOUNT_STR;


  -- From a string (euros/francs) return amount in euros
  function VALUE (STR : AMOUNT_STR) return OPER_DEF.AMOUNT_RANGE;


  -- Amount of an operation in LIST: -12345.12
  subtype SHORT_AMOUNT_STR is STRING (1 .. 9);
  -- From an amount (in euros) return 'image (euros/francs)
  -- Truncation rule:
  --  Sign is kept, three lower digits of unit removed,
  --  cents and dot replaced by " k "
  -- Result has first digit set to '-' or ' ' and aligned on right
  function SHORT_IMAGE (AMOUNT_IN_EUROS : OPER_DEF.AMOUNT_RANGE)
                       return SHORT_AMOUNT_STR;

end UNIT_FORMAT;

