with CALENDAR;
with PERPET, NORMAL, TEXT_HANDLER, DIR_MNG;
with PERS_DEF;
package body MESU_NAM is

  -- The result. File name (or file template)
  -- subtype FILE_NAME_STR is STRING (1 .. 12);


  -- "YYyyMmDd" or "        "
  -- subtype FILE_DATE_STR is STRING (1 .. 8);

  -- from "00" to "99" or "  "
  -- subtype FILE_NO_STR is STRING (1 .. 2);

  -- from "000" to "999" or "   "
  -- subtype FILE_PID_STR is STRING (1 .. 3);

  -- Date in file name : no hundredth of years
  subtype DATE_FOR_FILE_STR is STRING (1 .. 6);
  -- If a xx is less than DISCR then it is 20xx, else 19xx
  DISCRIMINANT_FOR_DATES : constant STRING (1 .. 2) := "90";

  function TRUNC (DATE_STR : FILE_DATE_STR) return DATE_FOR_FILE_STR is
    S : constant STRING(1 .. 6) := DATE_STR (3 .. 8);
  begin
    return S;
  end TRUNC;

  function ROUND (DATE_FOR_FILE : DATE_FOR_FILE_STR) return FILE_DATE_STR is
  begin
	if DATE_FOR_FILE (1 .. 2) < DISCRIMINANT_FOR_DATES then
	  return "20" & DATE_FOR_FILE;
	else
	  return "19" & DATE_FOR_FILE;
	end if;
  end ROUND;


  function VALID_DATE (DATE : FILE_DATE_STR) return BOOLEAN is
    YEAR  : CALENDAR.YEAR_NUMBER;
	MONTH : CALENDAR.MONTH_NUMBER;
	DAY  : CALENDAR.DAY_NUMBER;
  begin
	YEAR  := CALENDAR.YEAR_NUMBER'VALUE(DATE(1..4));
	MONTH := CALENDAR.MONTH_NUMBER'VALUE(DATE(5..6));
	DAY   := CALENDAR.DAY_NUMBER'VALUE(DATE(7..8));
    return DAY <= PERPET.NB_DAYS_MONTH (YEAR, MONTH);
  exception
    when others =>
	  return FALSE;
  end VALID_DATE;

  function VALID_NO (NO : STRING) return BOOLEAN is
  begin
	return NO'LENGTH = 2 and then NO >= "00" and then NO <= "99";
  exception
    when others =>
	  return FALSE;
  end VALID_NO;

  function VALID_PID (PID : in STRING) return BOOLEAN is
    PID_VAL : PERS_DEF.PID_RANGE;
  begin
    if PID'LENGTH /= 3 then
      return FALSE;
    end if;
	PID_VAL := PERS_DEF.PID_RANGE'VALUE(PID);
	return TRUE;
  exception
	when others =>
	  return FALSE;
  end VALID_PID;

  -- Check wether fields are valid
  function VALID_FILE_DEF (DATE : FILE_DATE_STR := WILD_DATE_STR;
                            NO   : FILE_NO_STR   := WILD_NO_STR;
                            PID  : FILE_PID_STR  := WILD_PID_STR)
   return BOOLEAN is
  begin
    if DATE /= WILD_DATE_STR and then not VALID_DATE (DATE) then
      return FALSE;
    elsif NO /= WILD_NO_STR and then not VALID_NO (NO) then
      return FALSE;
    elsif PID /= WILD_PID_STR and then not VALID_PID (PID) then
      return FALSE;
    else
      return TRUE;
    end if;
  exception
	when others =>
	  return FALSE;
   end VALID_FILE_DEF;

  -- Build a file name (or a template if some " ")
  -- May raise FILE_NAME_ERROR if some fields have wrong format
  --  or date is not valid
  function BUILD_FILE_NAME (DATE : FILE_DATE_STR := WILD_DATE_STR;
                            NO   : FILE_NO_STR   := WILD_NO_STR;
                            PID  : FILE_PID_STR  := WILD_PID_STR)
   return FILE_NAME_STR is
  begin
	if not VALID_FILE_DEF (DATE, NO, PID) then
	  raise FILE_NAME_ERROR;
	end if;
	if DATE = WILD_DATE_STR then
      return DATE(1..6) & NO & "." & PID;
	else
      return TRUNC(DATE) & NO & "." & PID;
	end if;
  end BUILD_FILE_NAME;

  -- Check wether fields are valid
  function VALID_FILE_NAME (FILE_NAME : FILE_NAME_STR) return BOOLEAN is
    DATE : FILE_DATE_STR;
  begin
	if FILE_NAME(9) /= '.' then
	  return FALSE;
	end if;

	if FILE_NAME(1 .. 6) = "??????" then
	  DATE := "??" & FILE_NAME;
	else
      DATE := ROUND (FILE_NAME(1 .. 6));
	  if not VALID_DATE (DATE) then
		return FALSE;
	  end if;
    end if;
      if FILE_NAME(7 .. 8) /= WILD_NO_STR
	  and then not VALID_NO (FILE_NAME(7 .. 8)) then
        return FALSE;
      end if;
	if FILE_NAME(10 .. 12) /= WILD_PID_STR
	and then not VALID_PID (FILE_NAME(10 .. 12)) then
	  return FALSE;
	end if;
	return TRUE;
  exception
	when others =>
	  return FALSE;
  end VALID_FILE_NAME;


  -- Split a file name (or a template)
  -- May raise FILE_NAME_ERROR if some fields have wrong format
  --  or date is not valid
  procedure SPLIT_FILE_NAME (FILE_NAME : in FILE_NAME_STR;
                             DATE      : out FILE_DATE_STR;
                             NO        : out FILE_NO_STR;
                             PID       : out FILE_PID_STR)  is
  begin
    if not VALID_FILE_NAME (FILE_NAME => FILE_NAME) then
      raise FILE_NAME_ERROR;
	end if;
	if FILE_NAME(1 .. 6) = "??????" then
	  DATE := "??" & FILE_NAME(1 .. 6);
	else
	  DATE := ROUND (FILE_NAME(1 .. 6));
	end if;

	NO := FILE_NAME(7 .. 8);
	PID := FILE_NAME(10 .. 12);
  end SPLIT_FILE_NAME;

  -- Find first file_no_str available for given date and pid
  -- May return WILD_NO_STR if no more_slot available
  -- May raise FILE_NAME_ERROR if date ir pid has wild
  function FIND_SLOT (DATE : FILE_DATE_STR;
                      PID  : FILE_PID_STR) return FILE_NO_STR is
    FILE_TEMPLATE : FILE_NAME_STR;
    LIST : DIR_MNG.FILE_LIST_MNG.LIST_TYPE;
    FILE : DIR_MNG.FILE_ENTRY_REC;
    L_DATE : FILE_DATE_STR;
    L_NO   : FILE_NO_STR;
    L_PID  : FILE_PID_STR;
    RET_NO : FILE_NO_STR;
    FILE_NAME : FILE_NAME_STR;

    function LESS_THAN (L, R : DIR_MNG.FILE_ENTRY_REC) return BOOLEAN is
    begin
      return L.NAME < R.NAME;
    end LESS_THAN;

    procedure MY_SORT is new DIR_MNG.FILE_LIST_MNG.SORT(LESS_THAN);

      use DIR_MNG.FILE_LIST_MNG;
  begin
    -- Check no wild_char
    if      TEXT_HANDLER.LOCATE (TEXT_HANDLER.TO_TEXT(DATE), WILD_CHAR) /= 0
    or else TEXT_HANDLER.LOCATE (TEXT_HANDLER.TO_TEXT(PID),  WILD_CHAR) /= 0 then
      raise FILE_NAME_ERROR;
    end if;
    -- build date??.pid
    FILE_TEMPLATE := BUILD_FILE_NAME (DATE => DATE, PID => PID);
    -- Search list of matching files
    DIR_MNG.LIST_DIR (LIST, "", FILE_TEMPLATE);

    -- Sort (by no because same date and pid)
    MY_SORT (LIST);
    -- Look for lowest
    RET_NO := "00";
    if IS_EMPTY (LIST) then
      return RET_NO;
    end if;
    MOVE_TO (LIST, NEXT, 0, FALSE);
    -- loop in list
    loop
      -- Get file name
      READ (LIST, FILE, CURRENT);
      FILE_NAME := FILE.NAME (1 .. FILE.LEN);
      SPLIT_FILE_NAME(FILE_NAME, L_DATE, L_NO, L_PID);

      if L_NO /= RET_NO then
        -- Ret_no is an empty slot
        exit;
      elsif RET_NO = "99" then
        -- No free slot
        RET_NO := WILD_NO_STR;
        exit;
      else
        -- Next slot
        RET_NO := NORMAL(INTEGER'VALUE(RET_NO) + 1, 2, GAP => '0');
        if GET_POSITION(LIST) = LIST_LENGTH(LIST) then
          -- End of list
          exit;
        else
          -- Go to next entry
          MOVE_TO (LIST);
        end if;
      end if;

    end loop;

    -- Done. Garbage collect!
    DELETE_LIST (LIST);
    return RET_NO;
  end FIND_SLOT;



end MESU_NAM;
