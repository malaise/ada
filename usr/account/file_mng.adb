with TEXT_IO, SEQUENTIAL_IO;
with ADA.EXCEPTIONS;
with DIRECTORY, TEXT_HANDLER;

with OPER_DEF;
package body FILE_MNG is

  package OPER_IO is new SEQUENTIAL_IO(OPER_DEF.OPER_REC);


  -- First record in file as to be this
  --  except amount which is the account amount
  MAGIC_OPER : OPER_DEF.OPER_REC;


  -- Overwrites the list from file content
  procedure LOAD (FILE_NAME : in STRING;
                  OPER_LIST : in out OPER_LIST_MNG.LIST_TYPE) is
    FILE : OPER_IO.FILE_TYPE;
    -- A record to read
    LOC_OPER : OPER_DEF.OPER_REC; 
    -- A list for tempo read
    LOC_LIST : OPER_LIST_MNG.LIST_TYPE;
    use OPER_DEF;
  begin
    begin
      OPER_IO.OPEN (FILE, OPER_IO.IN_FILE, FILE_NAME);
    exception
      when others => raise F_ACCESS_ERROR;
    end;
    OPER_IO.RESET (FILE);

    -- Read magic record
    begin
      OPER_IO.READ (FILE, LOC_OPER);
    exception
      when others =>
        -- There should be at least the magic record
        OPER_IO.CLOSE (FILE);
        raise F_ACCESS_ERROR;
    end;
    LOC_OPER.AMOUNT := MAGIC_OPER.AMOUNT;
    if LOC_OPER /= MAGIC_OPER then
      -- Bad magic record
      OPER_IO.CLOSE (FILE);
      raise F_ACCESS_ERROR;
    end if;

    -- Read records from file to list
    OPER_IO.RESET (FILE);
    loop
      begin
        OPER_IO.READ (FILE, LOC_OPER);
      exception
        when OPER_IO.END_ERROR =>
          exit;
      end;
      OPER_LIST_MNG.INSERT (LOC_LIST, LOC_OPER);
    end loop;

    -- Everything OK. Overwrite the existing list. Go to end.
    OPER_LIST_MNG.DELETE_LIST (OPER_LIST);
    OPER_LIST_MNG.ASSIGN (OPER_LIST, LOC_LIST);
    OPER_LIST_MNG.MOVE_TO (OPER_LIST, OPER_LIST_MNG.PREV, 0, FALSE);

    OPER_IO.CLOSE (FILE);
    
  exception
    when F_ACCESS_ERROR =>
      raise;
    when FILE_ERROR : others =>
      TEXT_IO.PUT_LINE ("Exception " & ADA.EXCEPTIONS.EXCEPTION_NAME(FILE_ERROR)
                      & " raised while loading file " & FILE_NAME);
      begin
        OPER_IO.CLOSE (FILE);
      exception
        when others => null;
      end;
      -- Clean the garbage
      OPER_LIST_MNG.DELETE_LIST (LOC_LIST);
      raise F_IO_ERROR;
  end LOAD;

  -- Save the list in file
  procedure SAVE (FILE_NAME : in STRING;
                  OPER_LIST : in OPER_LIST_MNG.LIST_TYPE) is
    -- Position in list
    LOC_POS : NATURAL; 
    FILE : OPER_IO.FILE_TYPE;
    -- A record to write
    LOC_OPER, LOC_OPER_1 : OPER_DEF.OPER_REC; 
    -- A list for tempo move
    LOC_LIST : OPER_LIST_MNG.LIST_TYPE;
  begin
    OPER_LIST_MNG.ASSIGN (LOC_LIST, OPER_LIST);

    -- Save current position
    LOC_POS := OPER_LIST_MNG.GET_POSITION (LOC_LIST);

    -- Create / erase file
    begin
      OPER_IO.OPEN(FILE, OPER_IO.OUT_FILE, FILE_NAME);
      OPER_IO.DELETE(FILE);
      OPER_IO.CREATE(FILE, OPER_IO.OUT_FILE, FILE_NAME);
    exception
      when OPER_IO.NAME_ERROR =>
        -- New file
        begin
          OPER_IO.CREATE (FILE, OPER_IO.OUT_FILE, FILE_NAME);
        exception
          when others =>
            -- Cannot create
            raise F_ACCESS_ERROR;
        end;
      when others =>
        -- Other error on open
        raise F_ACCESS_ERROR;
    end;

    -- Rewind, write magic record with amount of first record
    OPER_LIST_MNG.MOVE_TO (LOC_LIST, OPER_LIST_MNG.NEXT, 0, FALSE);
    OPER_LIST_MNG.READ (LOC_LIST, LOC_OPER, OPER_LIST_MNG.CURRENT);
    LOC_OPER_1 := MAGIC_OPER;
    LOC_OPER_1.AMOUNT := LOC_OPER.AMOUNT;
    OPER_IO.WRITE (FILE, LOC_OPER_1);

    -- Write other records
    loop
      begin
        OPER_LIST_MNG.MOVE_TO (LOC_LIST, OPER_LIST_MNG.NEXT);
      exception
        when OPER_LIST_MNG.NOT_IN_LIST =>
          exit;
      end;
      OPER_LIST_MNG.READ (LOC_LIST, LOC_OPER, OPER_LIST_MNG.CURRENT);
      OPER_IO.WRITE (FILE, LOC_OPER);
    end loop;

    -- Done. Close file, move to saved position
    OPER_IO.CLOSE (FILE);
    OPER_LIST_MNG.MOVE_TO (LOC_LIST, OPER_LIST_MNG.NEXT, LOC_POS-1, FALSE);

  exception
    when F_ACCESS_ERROR =>
      raise;
    when FILE_ERROR : others =>
      TEXT_IO.PUT_LINE ("Exception " & ADA.EXCEPTIONS.EXCEPTION_NAME(FILE_ERROR)
                      & " raised while closing file " & FILE_NAME);
      begin
        OPER_IO.CLOSE (FILE);
      exception
        when others => null;
      end;
      raise F_IO_ERROR;
  end SAVE;

end FILE_MNG;

