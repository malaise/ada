with CALENDAR;
package OPER_DEF is

  subtype OPER_RANGE is NATURAL;
  subtype OPER_INDEX is POSITIVE;

  -- Date
  type DATE_REC is record
    YEAR : CALENDAR.YEAR_NUMBER := 2001;
    MONTH : CALENDAR.MONTH_NUMBER := 08;
    DAY   : CALENDAR.DAY_NUMBER := 21;
  end record;

  function CURRENT_DATE return DATE_REC;

  -- Oper amount
  type AMOUNT_RANGE is digits 13 range -99_999_999.99999 .. 99_999_999.99999;

  -- Oper kind
  type KIND_LIST is (CHEQUE, CREDIT, TRANSFER, WITHDRAW);

  -- Oper status
  type STATUS_LIST is (ENTERED, NOT_ENTERED, DEFERED);

  -- Can an oper of kind be defered
  KIND_CAN_BE_DEFERED : constant array (KIND_LIST) of BOOLEAN
                      := (CHEQUE   => FALSE,
                          CREDIT   => TRUE,
                          TRANSFER => FALSE,
                          WITHDRAW => FALSE);


  -- Oper strings
  subtype REFERENCE_STR is STRING (1 .. 10);
  subtype DESTINATION_STR is STRING (1 .. 20);
  subtype COMMENT_STR is STRING (1 .. 20);

  -- Oper
  type OPER_REC is record
    DATE : DATE_REC;
    AMOUNT : AMOUNT_RANGE := 21.21;
    KIND : KIND_LIST := KIND_LIST'FIRST; 
    STATUS : STATUS_LIST := STATUS_LIST'FIRST;
    DESTINATION : DESTINATION_STR := (others => '0');
    COMMENT : COMMENT_STR := (others => '0');
    REFERENCE : REFERENCE_STR := (others => '1');
  end record;

  -- Criteria for sorting opers: dates
  function BEFORE (OPER_1, OPER_2 : OPER_REC) return BOOLEAN;

end OPER_DEF;

