with MESU_DEF;
package MESU_NAM is

  -- The result. File name (or file template)
  subtype FILE_NAME_STR is STRING (1 .. 12);

  WILD_CHAR : constant CHARACTER := '?';

  -- "YYyyMmDd" or "????????"
  subtype FILE_DATE_STR is MESU_DEF.DATE_STR;
  WILD_DATE_STR : constant FILE_DATE_STR := (others => WILD_CHAR);

  -- from "00" to "99" or "??"
  subtype FILE_NO_STR is STRING (1 .. 2);
  WILD_NO_STR : constant FILE_NO_STR := (others => WILD_CHAR);

  -- from "000" to "999" or "???"
  subtype FILE_PID_STR is STRING (1 .. 3);
  WILD_PID_STR : constant FILE_PID_STR := (others => WILD_CHAR);


  -- Build a file name (or a template if some '?')
  -- May raise FILE_NAME_ERROR if some fields have wrong format
  --  or date is not valid
  function BUILD_FILE_NAME (DATE : FILE_DATE_STR := WILD_DATE_STR;
                            NO   : FILE_NO_STR   := WILD_NO_STR;
                            PID  : FILE_PID_STR  := WILD_PID_STR)
   return FILE_NAME_STR;

  -- Check wether fields are valid
  function VALID_FILE_DEF (DATE : FILE_DATE_STR := WILD_DATE_STR;
                           NO   : FILE_NO_STR   := WILD_NO_STR;
                           PID  : FILE_PID_STR  := WILD_PID_STR)
   return BOOLEAN;

  -- Split a file name (or a template)
  -- May raise FILE_NAME_ERROR if some fields have wrong format
  --  or date is not valid
  procedure SPLIT_FILE_NAME (FILE_NAME : in FILE_NAME_STR;
                             DATE      : out FILE_DATE_STR;
                             NO        : out FILE_NO_STR;
                             PID       : out FILE_PID_STR);

  -- Check wether fields are valid
  function VALID_FILE_NAME (FILE_NAME : FILE_NAME_STR) return BOOLEAN;

  -- Find first file_no_str available for given date and pid
  -- May return WILD_NO_STR if no more_slot available
  -- May raise FILE_NAME_ERROR if date ir pid has wild
  function FIND_SLOT (DATE : FILE_DATE_STR;
                      PID  : FILE_PID_STR) return FILE_NO_STR;


  FILE_NAME_ERROR : exception;

end MESU_NAM;