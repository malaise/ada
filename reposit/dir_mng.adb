package body DIR_MNG is

  DIR_INTERNAL_ERROR : exception;

  PATH_SEPARATOR : constant CHARACTER := '/';

  function LESS_THAN (EL1, EL2 : in FILE_ENTRY_REC) return BOOLEAN is
    use DIRECTORY;
  begin
    -- Only one is dir
    if EL1.KIND /= EL2.KIND and then
     (EL1.KIND = DIRECTORY.DIR or else EL2.KIND = DIRECTORY.DIR) then
      return EL1.KIND = DIRECTORY.DIR;
    else
      return EL1.NAME (1 .. EL1.LEN) < EL2.NAME (1 .. EL2.LEN);
    end if;
  end LESS_THAN;


  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in STRING := "";
                      TEMPLATE : in STRING := "") is
    DIR_DESC : DIRECTORY.DIR_DESC;
    FILE_REC : FILE_ENTRY_REC;
    FILE_NAME : FILE_TXT;
    FILE_RIGHTS : NATURAL;
  begin

    if DIR = "" then
      DIR_DESC := DIRECTORY.OPEN (".");
    else
      DIR_DESC := DIRECTORY.OPEN (DIR);
    end if;

    loop
      TEXT_HANDLER.SET (FILE_NAME, DIRECTORY.NEXT_ENTRY (DIR_DESC));

      if TEMPLATE = ""
      or else DIRECTORY.FILE_MATCH(TEXT_HANDLER.VALUE (FILE_NAME),
                                   TEMPLATE) then
        FILE_REC.LEN := TEXT_HANDLER.LENGTH (FILE_NAME);
        FILE_REC.NAME (1 .. FILE_REC.LEN) := TEXT_HANDLER.VALUE (FILE_NAME);
        if DIR = "" then
          DIRECTORY.FILE_STAT (
           TEXT_HANDLER.VALUE (FILE_NAME),
           FILE_REC.KIND, FILE_RIGHTS);
        else
          DIRECTORY.FILE_STAT (
           DIR & PATH_SEPARATOR & TEXT_HANDLER.VALUE (FILE_NAME),
           FILE_REC.KIND, FILE_RIGHTS);
        end if;
        FILE_LIST_MNG.INSERT (LIST => LIST,
                              ITEM => FILE_REC,
                              WHERE => FILE_LIST_MNG.NEXT);
      end if;
    end loop;
  exception
    when DIRECTORY.END_ERROR =>
      DIRECTORY.CLOSE (DIR_DESC);
  end LIST_DIR;

  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in FILE_TXT := TEXT_HANDLER.EMPTY_TEXT;
                      TEMPLATE : in FILE_TXT := TEXT_HANDLER.EMPTY_TEXT) is
  begin
    LIST_DIR (LIST, TEXT_HANDLER.VALUE(DIR), TEXT_HANDLER.VALUE(TEMPLATE));
  end LIST_DIR;


end DIR_MNG;

