with SYSTEM;
with INTERFACES.C_STREAMS;
with INTERFACES.C.STRINGS;

package body SYS_CALLS is

  function C_STRLEN (S : SYSTEM.ADDRESS) return NATURAL;
  pragma IMPORT (C, C_STRLEN, "strlen");

  function C_STRCPY (DEST, SRC : SYSTEM.ADDRESS) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_STRCPY, "strcpy");

  function C_MEMCPY (DEST, SRC : SYSTEM.ADDRESS; SIZE : INTEGER)
  return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_MEMCPY, "memcpy");

  function STR_FOR_C (STR : STRING) return STRING is
  begin
    return STR & ASCII.NUL;
  end STR_FOR_C;

  function STR_FROM_C (STR_ADDR : INTERFACES.C.STRINGS.CHARS_PTR) return STRING is
  begin
    return INTERFACES.C.STRINGS.VALUE(STR_ADDR);
  exception
    when INTERFACES.C.STRINGS.DEREFERENCE_ERROR =>
      return "";
  end STR_FROM_C;

  function CALL_SYSTEM (COMMAND : STRING) return INTEGER is
    COMMAND4C : constant STRING := STR_FOR_C (COMMAND);

    function C_SYSTEM (COMMAND : SYSTEM.ADDRESS) return INTEGER;
    pragma IMPORT (C, C_SYSTEM, "system");

  begin
    return C_SYSTEM (COMMAND4C'ADDRESS);
  end CALL_SYSTEM;

  function UNLINK (FILE_NAME : STRING) return BOOLEAN is
    FILE_NAME4C : constant STRING := STR_FOR_C (FILE_NAME);
    RES : INTEGER;

    function C_UNLINK (PATHNAME: SYSTEM.ADDRESS) return INTEGER;
    pragma IMPORT (C, C_UNLINK, "unlink");

  begin
    RES := C_UNLINK (FILE_NAME4C'ADDRESS);
    return RES = 0;
  end UNLINK;  

  function RENAME (SRC, DEST : STRING) return BOOLEAN is
    SRC4C : constant STRING := STR_FOR_C (SRC);
    DEST4C : constant STRING := STR_FOR_C (DEST);
    RES : INTEGER;

    function C_RENAME (OLDPATH, NEWPATH : SYSTEM.ADDRESS) return INTEGER;
    pragma IMPORT (C, C_RENAME, "rename");
  begin
    RES := C_RENAME (SRC4C'ADDRESS, DEST4C'ADDRESS);
    return RES = 0;
  end RENAME;

  function ERRNO return INTEGER is
    function C_GET_ERRNO return INTEGER;
    pragma IMPORT (C, C_GET_ERRNO, "__get_errno");
  begin
    return C_GET_ERRNO;
  end ERRNO;
   
 
  function STR_ERROR (ERR : INTEGER) return STRING is

    function C_STRERROR (ERRNUM: INTEGER) return INTERFACES.C.STRINGS.CHARS_PTR;
    pragma IMPORT (C, C_STRERROR, "strerror");

  begin
    return STR_FROM_C (C_STRERROR (ERR));
  end STR_ERROR;


  procedure PUT_ERROR (STR : in STRING) is
    I : INTERFACES.C_STREAMS.INT;
    C_STR : constant STRING := STR & ASCII.NUL;
  begin
    I := INTERFACES.C_STREAMS.FPUTS (C_STR'ADDRESS,
                 INTERFACES.C_STREAMS.STDERR);
  end PUT_ERROR;

  procedure NEW_LINE_ERROR is
    I : INTERFACES.C_STREAMS.INT;
    C_STR : constant STRING := ASCII.LF & ASCII.NUL;
  begin
    I := INTERFACES.C_STREAMS.FPUTS (C_STR'ADDRESS,
                 INTERFACES.C_STREAMS.STDERR);
  end NEW_LINE_ERROR;

  procedure PUT_LINE_ERROR (STR : in STRING) is
  begin
    PUT_ERROR (STR);
    NEW_LINE_ERROR;
  end PUT_LINE_ERROR;

  -- Getenv and truncates if necessary
  procedure GETENV (ENV_NAME : in STRING;
                    ENV_SET   : out BOOLEAN;
                    ENV_TRUNC : out BOOLEAN;
                    ENV_VALUE : out STRING;
                    ENV_LEN   : out NATURAL) is
    NAME4C : constant STRING := STR_FOR_C (ENV_NAME);
    ADDR : SYSTEM.ADDRESS;

    function C_GETENV (NAME : in SYSTEM.ADDRESS) return SYSTEM.ADDRESS;
    pragma IMPORT (C, C_GETENV, "getenv");

    use SYSTEM;
  begin
    ADDR := C_GETENV (NAME4C'ADDRESS);
    if ADDR = SYSTEM.NULL_ADDRESS then
      ENV_SET := FALSE;
      ENV_TRUNC := FALSE;
      ENV_LEN := 0;
      return;
    end if;

    ENV_SET := TRUE;
    declare
      RESULT : STRING (1 .. C_STRLEN(ADDR));
      DUMMY_ADDR : SYSTEM.ADDRESS;
    begin
      DUMMY_ADDR := C_MEMCPY (RESULT'ADDRESS, ADDR, RESULT'LENGTH);
      if RESULT'LENGTH <= ENV_VALUE'LENGTH then
        ENV_TRUNC := FALSE;
        ENV_LEN := RESULT'LENGTH;
        ENV_VALUE (ENV_VALUE'FIRST .. ENV_VALUE'FIRST + RESULT'LENGTH - 1) := RESULT;
      else
        ENV_TRUNC := TRUE;
        ENV_LEN := ENV_VALUE'LENGTH;
        ENV_VALUE := RESULT (RESULT'FIRST .. RESULT'FIRST + ENV_VALUE'LENGTH - 1);
      end if;
    end;
  end GETENV;
end SYS_CALLS; 

 
