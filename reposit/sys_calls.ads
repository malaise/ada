package SYS_CALLS is

  -- Call system
  function CALL_SYSTEM (COMMAND : STRING) return INTEGER;

  -- Unlink a file
  function UNLINK (FILE_NAME : STRING) return BOOLEAN;

  -- Rename/move a file
  function RENAME (SRC, DEST : STRING) return BOOLEAN;

  -- Errno and associated string
  function ERRNO return INTEGER;
  function STR_ERROR (ERR : INTEGER) return STRING;

  -- Put line on stderr
  procedure PUT_ERROR (STR : in STRING);
  procedure PUT_LINE_ERROR (STR : in STRING);
  procedure NEW_LINE_ERROR;

  -- Getenv and truncates if necessary
  procedure GETENV (ENV_NAME : in STRING;
                    ENV_SET   : out BOOLEAN;
                    ENV_TRUNC : out BOOLEAN;
                    ENV_VALUE : out STRING;
                    ENV_LEN   : out NATURAL);

  -- Set error exit code
  procedure SET_ERROR_EXIT_CODE;

end SYS_CALLS; 

 
