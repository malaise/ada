with OPER_LIST_MNG;
package FILE_MNG is


  -- Overwrites the list from file content
  procedure LOAD (FILE_NAME : in STRING;
                  OPER_LIST : in out OPER_LIST_MNG.LIST_TYPE;
                  CAN_WRITE : out BOOLEAN);

  -- Save the list in file
  procedure SAVE (FILE_NAME : in STRING;
                  OPER_LIST : in OPER_LIST_MNG.LIST_TYPE);

  -- File not found on load, or protected on save
  F_ACCESS_ERROR : exception;
  -- Read/Write error
  F_IO_ERROR     : exception;

end FILE_MNG;

