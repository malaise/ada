with SYS_CALLS, BIT_OPS;
package body DIRECTORY is
  use SYSTEM;

  subtype DIR_STR is STRING (1 .. 256);

  SEPARATOR : constant CHARACTER := '/';

  type C_DIRENT_REC is record
    D_INO : LONG_INTEGER;
    D_OFF : LONG_INTEGER;
    D_RECLEN : SHORT_INTEGER;
    D_NAME : DIR_STR;
  end record;

  type C_STAT_REC is record
    ST_DEV : SHORT_INTEGER;
    ST_INO : LONG_INTEGER;
    ST_MODE : SHORT_INTEGER;
    ST_LINK : SHORT_INTEGER;
    ST_UID : SHORT_INTEGER;
    ST_GID : SHORT_INTEGER;
    ST_SIZE : LONG_INTEGER;
    ST_RDEV : SHORT_INTEGER;
    ST_BLOCKSIZE : LONG_INTEGER;
    ST_BLOCKS : LONG_INTEGER;
    ST_ATIME : LONG_INTEGER;
    ST_CTIME : LONG_INTEGER;
  end record;

  function C_STRLEN (S : SYSTEM.ADDRESS) return NATURAL;
  pragma IMPORT (C, C_STRLEN, "strlen");

  function C_STRCPY (DEST, SRC : SYSTEM.ADDRESS) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_STRCPY, "strcpy");

  function C_MEMCPY (DEST, SRC : SYSTEM.ADDRESS; SIZE : INTEGER) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_MEMCPY, "memcpy");


  function STR_FOR_C (STR : STRING) return STRING is
  begin
    return STR & ASCII.NUL;
  end STR_FOR_C;


  function C_GETCWD (BUF : SYSTEM.ADDRESS; SIZE : INTEGER) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_GETCWD, "getcwd");

  -- Returns current working directory
  function GET_CURRENT return STRING is
    ADDR : SYSTEM.ADDRESS;
    RESULT : STRING (1 .. MAX_DIR_NAME_LEN);
    LEN : NATURAL;
  begin
    ADDR := C_GETCWD (RESULT(RESULT'FIRST)'ADDRESS, RESULT'LENGTH);
    if ADDR = SYSTEM.NULL_ADDRESS then
      -- Buffer too small
      raise CONSTRAINT_ERROR;
    end if;
    LEN := C_STRLEN (RESULT(RESULT'FIRST)'ADDRESS);
    return RESULT (1 .. LEN);
  end GET_CURRENT;

  procedure GET_CURRENT (CUR_DIR : in out TEXT_HANDLER.TEXT) is
  begin
    TEXT_HANDLER.SET (CUR_DIR, GET_CURRENT);
  end GET_CURRENT;


  function C_CHDIR (PATH : SYSTEM.ADDRESS) return INTEGER;
  pragma IMPORT (C, C_CHDIR, "chdir");

  -- Changes current working directory
  procedure CHANGE_CURRENT (NEW_DIR : in STRING) is
    C_NEW_DIR : constant STRING := STR_FOR_C(NEW_DIR);
  begin
    if C_CHDIR (C_NEW_DIR'ADDRESS) = -1 then
      raise NAME_ERROR;
    end if;
  end CHANGE_CURRENT;


  function C_OPENDIR (NAME : SYSTEM.ADDRESS) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_OPENDIR, "opendir");

  -- Opens a directory for list of entries
  function OPEN (DIR_NAME : in STRING) return DIR_DESC is
    C_DIR_NAME : constant STRING := STR_FOR_C(DIR_NAME);
    DESC : DIR_DESC;
  begin
    if DESC.DIR_ADDR /= SYSTEM.NULL_ADDRESS then
      raise OPEN_ERROR;
    end if;
    DESC.DIR_ADDR := C_OPENDIR (C_DIR_NAME'ADDRESS);
    if DESC.DIR_ADDR = SYSTEM.NULL_ADDRESS then
      raise NAME_ERROR;
    end if;
    return DESC;
  end OPEN;


  function C_READDIR (DIR : SYSTEM.ADDRESS) return SYSTEM.ADDRESS;
  pragma IMPORT (C, C_READDIR, "readdir");

  -- Gets next entry of the opened directory
  function NEXT_ENTRY (DESC : DIR_DESC) return STRING is
    DIRENT : C_DIRENT_REC;
    ADDR, DUMMY : SYSTEM.ADDRESS;
    LEN : NATURAL;
  begin
    -- Check dir desc
    if DESC.DIR_ADDR = SYSTEM.NULL_ADDRESS then
      raise OPEN_ERROR;
    end if;

    -- Read entry and check validity
    ADDR := C_READDIR (DESC.DIR_ADDR);
    if ADDR = SYSTEM.NULL_ADDRESS then
      raise END_ERROR;
    end if;

    -- Copy address to record
    DUMMY := C_MEMCPY (DIRENT'ADDRESS, ADDR, DIRENT'SIZE/SYSTEM.STORAGE_UNIT);
    LEN := C_STRLEN(DIRENT.D_NAME'ADDRESS);
    -- Done
    return DIRENT.D_NAME (1 .. LEN);
  end NEXT_ENTRY;

  procedure NEXT_ENTRY (DESC : in DIR_DESC; DIR_ENTRY : in out TEXT_HANDLER.TEXT) is
  begin
    TEXT_HANDLER.SET (DIR_ENTRY, NEXT_ENTRY (DESC));
  end NEXT_ENTRY;


  procedure C_REWINDDIR (DIR : SYSTEM.ADDRESS);
  pragma IMPORT (C, C_REWINDDIR, "rewinddir");

  -- Reset entries for the first 
  procedure REWIND (DESC : in DIR_DESC) is
  begin
    -- Check dir desc
    if DESC.DIR_ADDR = SYSTEM.NULL_ADDRESS then
      raise OPEN_ERROR;
    end if;
    C_REWINDDIR (DESC.DIR_ADDR);
  end REWIND;


  procedure C_CLOSEDIR (DIR : SYSTEM.ADDRESS);
  pragma IMPORT (C, C_CLOSEDIR, "closedir");

  -- Closes a directory
  procedure CLOSE (DESC : in out DIR_DESC) is
  begin
    if DESC.DIR_ADDR = SYSTEM.NULL_ADDRESS then
      raise OPEN_ERROR;
    end if;
    C_CLOSEDIR (DESC.DIR_ADDR);
    DESC.DIR_ADDR := SYSTEM.NULL_ADDRESS;
  end CLOSE;


  function C_STAT (FILE_NAME : SYSTEM.ADDRESS; STAT : SYSTEM.ADDRESS)
  return INTEGER;
  pragma IMPORT (C, C_STAT, "lstat");

  -- type FILE_KIND_LIST is (FILE, DIR, DEVICE, FIFO_SOCKET);
  procedure FILE_STAT (FILE_NAME : in STRING;
                       KIND : out FILE_KIND_LIST; RIGHTS : out NATURAL) is
    C_FILE_NAME : constant STRING := STR_FOR_C (FILE_NAME);
    STAT : C_STAT_REC;
    RES : INTEGER;
    MODE : INTEGER;
    use BIT_OPS;
  begin
    RES := C_STAT(C_FILE_NAME(C_FILE_NAME'FIRST)'ADDRESS, STAT'ADDRESS);
    if RES = -1 then
      raise NAME_ERROR;
    end if;
    MODE := INTEGER(STAT.ST_MODE) AND 8#00170000#;
    MODE := SHR (MODE, 12);
    case MODE is
      when 8#14# =>
        KIND := SOCKET;
      when 8#12# =>
        KIND := SYMBOLIC_LINK;
      when 8#10# =>
        KIND := FILE;
      when 8#06# =>
        KIND := BLOCK_DEVICE;
      when 8#04# =>
        KIND := DIR;
      when 8#02# =>
        KIND := CHARACTER_DEVICE;
      when 8#01# =>
        KIND := FIFO;
      when others =>
        KIND := UNKNOWN;
    end case;
    RIGHTS := INTEGER(STAT.ST_MODE) AND 8#00007777#;
  end FILE_STAT;


  function C_READLINK (PATH : SYSTEM.ADDRESS;
                       BUF : SYSTEM.ADDRESS; BUFSIZ : INTEGER) return INTEGER;
  pragma IMPORT (C, C_READLINK, "readlink");

  -- May raise NAME_ERROR if FILE_NAME does not exist
  --           OPEN_ERROR if FILE_NAME is not a link
  function READ_ONE_LINK (FILE_NAME : STRING) return STRING is
    STR : STRING(1 .. MAX_DIR_NAME_LEN);
    C_FILE_NAME : constant STRING := STR_FOR_C(FILE_NAME);
    RES : INTEGER;
  begin
    RES := C_READLINK (C_FILE_NAME'ADDRESS, STR'ADDRESS, STR'LENGTH);
    if RES /= -1 then
      return STR (1 .. RES);
    elsif SYS_CALLS.ERRNO = 2 then
      -- ENOENT : file not found
      raise NAME_ERROR;
    else
      raise OPEN_ERROR;
    end if;
  end READ_ONE_LINK;

  procedure EXTRACT_PATH (FROM : in STRING; TO : in out TEXT_HANDLER.TEXT) is
  begin
    if FROM(FROM'FIRST) /= SEPARATOR then
      TEXT_HANDLER.EMPTY(TO);
      return;
    end if;
    for I in reverse FROM'RANGE loop
      if FROM(I) = SEPARATOR then
        TEXT_HANDLER.SET (TO, FROM(FROM'FIRST .. I - 1));
        exit;
      end if;
    end loop;
  end EXTRACT_PATH;

  function READ_LINK (FILE_NAME : STRING;
                      RECURSIVE : BOOLEAN := TRUE) return STRING is
    DIR, TXT : TEXT_HANDLER.TEXT(MAX_DIR_NAME_LEN);
    KIND : FILE_KIND_LIST;
    RIGHTS : NATURAL;
    use TEXT_HANDLER;
  begin
    -- Check file_name  is a link
    FILE_STAT (FILE_NAME, KIND, RIGHTS);
    if KIND /= SYMBOLIC_LINK then
      raise OPEN_ERROR;
    end if;
    if not RECURSIVE then
      return READ_ONE_LINK(FILE_NAME);
    end if;

    -- Prepend current dir if relative, store path
    TEXT_HANDLER.SET (TXT, FILE_NAME);
    if TEXT_HANDLER.VALUE(TXT)(1) /= SEPARATOR then
      GET_CURRENT(DIR);
      TEXT_HANDLER.SET (TXT, DIR & '/' & TXT);
    end if;
    EXTRACT_PATH(TEXT_HANDLER.VALUE(TXT), DIR);

    loop
      -- Current is a link
      TEXT_HANDLER.SET (TXT, READ_ONE_LINK(TEXT_HANDLER.VALUE(TXT)));

      -- Prepend path if relative, store path
      if TEXT_HANDLER.VALUE(TXT)(1) /= SEPARATOR then
        TEXT_HANDLER.SET (TXT, DIR & '/' & TXT);
      end if;
      EXTRACT_PATH(TEXT_HANDLER.VALUE(TXT), DIR);
      
      FILE_STAT(TEXT_HANDLER.VALUE(TXT), KIND, RIGHTS);
      exit when KIND /= SYMBOLIC_LINK;
    end loop;
    return TEXT_HANDLER.VALUE(TXT);
  end READ_LINK;

  procedure READ_LINK (FILE_NAME : in STRING;
                       TARGET : in out TEXT_HANDLER.TEXT;
                       RECURSIVE : in BOOLEAN := TRUE) is
  begin
    TEXT_HANDLER.SET (TARGET, READ_LINK(FILE_NAME, RECURSIVE));
  end READ_LINK;


  function C_FNMATCH (PATTERN : SYSTEM.ADDRESS; STRINGS : SYSTEM.ADDRESS;
                      FLAGS : INTEGER) return INTEGER;
  pragma IMPORT (C, C_FNMATCH, "fnmatch");

  -- Does file name match a pattern
  function FILE_MATCH (FILE_NAME : STRING; TEMPLATE : STRING) return BOOLEAN is
    C_FILE_NAME : constant STRING := STR_FOR_C (FILE_NAME);
    C_TEMPLATE : constant STRING := STR_FOR_C (TEMPLATE);
  begin
    return C_FNMATCH(C_TEMPLATE'ADDRESS, C_FILE_NAME'ADDRESS, 0) = 0;
  end FILE_MATCH;
end DIRECTORY;


