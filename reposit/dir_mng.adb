package body DIR_MNG is

  DIR_INTERNAL_ERROR : exception;

  function LESS_THAN (EL1, EL2 : in FILE_ENTRY_REC) return BOOLEAN is
  begin
    return EL1.NAME (1 .. EL1.LEN) < EL2.NAME (1 .. EL2.LEN);
  end LESS_THAN;

  procedure FILE_SORT is new FILE_LIST_MNG.SORT(LESS_THAN);

  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in STRING := "") is
    DIR_DESC : DIRECTORY.DIR_DESC;
    FILE_REC : FILE_ENTRY_REC;
    FILE_NAME : FILE_TXT;
  begin

    if DIR = "" then
      DIR_DESC := DIRECTORY.OPEN (".");
    else
      DIR_DESC := DIRECTORY.OPEN (DIR);
    end if;

    loop
      TEXT_HANDLER.SET (FILE_NAME, DIRECTORY.NEXT_ENTRY (DIR_DESC));
      FILE_REC.LEN := TEXT_HANDLER.LENGTH (FILE_NAME);
      FILE_REC.NAME (1 .. FILE_REC.LEN) := TEXT_HANDLER.VALUE (FILE_NAME);

      FILE_LIST_MNG.INSERT (LIST => LIST,
                            ITEM => FILE_REC,
                            WHERE => FILE_LIST_MNG.NEXT);
    end loop;
  exception
    when DIRECTORY.END_ERROR =>
      DIRECTORY.CLOSE (DIR_DESC);
  end LIST_DIR;

  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in FILE_TXT := TEXT_HANDLER.EMPTY_TEXT) is
  begin
    LIST_DIR (LIST, TEXT_HANDLER.VALUE(DIR));
  end LIST_DIR;


end DIR_MNG;

