with TEXT_HANDLER;
with DYNAMIC_LIST;
with DIRECTORY;
package DIR_MNG is

  subtype FILE_STR_RANGE is POSITIVE range 1 .. DIRECTORY.MAX_DIR_NAME_LEN;
  subtype FILE_STR is STRING (FILE_STR_RANGE);
  subtype FILE_TXT is TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN);

  type FILE_ENTRY_REC is record
    NAME : FILE_STR;
    LEN  : FILE_STR_RANGE;
  end record;
  package FILE_LIST_MNG is new DYNAMIC_LIST (ELEMENT_TYPE => FILE_ENTRY_REC);

  -- List files of a <drive>:<path>
  --  and append them at the end of the current list
  -- Current is set to the last item appended or not changed if no file found.
  -- "." and ".." are not inserted in the list
  -- If DIR is empty, then current drive and path and *.* is assumed
  -- May raise PATH_ERROR if DIR is not valid or not existing
  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in STRING := "";
                      TEMPLATE : in STRING := "");
  procedure LIST_DIR (LIST : in out FILE_LIST_MNG.LIST_TYPE;
                      DIR  : in FILE_TXT := TEXT_HANDLER.EMPTY_TEXT;
                      TEMPLATE : in FILE_TXT := TEXT_HANDLER.EMPTY_TEXT);

  -- To sort files. Directories, then hidden, then system, then others.
  -- By prefix then suffix
  function LESS_THAN (EL1, EL2 : in FILE_ENTRY_REC) return BOOLEAN;

  NAME_ERROR : exception renames DIRECTORY.NAME_ERROR;

end DIR_MNG;

