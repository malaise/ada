with DIRECT_IO;
with PERS_DEF;
use PERS_DEF;
package body PERS_FIL is

  -- Direct_io of persons
  package PERSON_IO is new DIRECT_IO (ELEMENT_TYPE => PERS_DEF.PERSON_REC);
  PERSON_FILE : PERSON_IO.FILE_TYPE;
  PERSON_FILE_NAME : constant STRING := "PERSONS.LST";


  procedure OPEN is
  begin
    -- Try to open existing file
    begin
      PERSON_IO.OPEN (PERSON_FILE, PERSON_IO.INOUT_FILE, PERSON_FILE_NAME);
    exception
      when PERSON_IO.NAME_ERROR =>
        PERSON_IO.CREATE (PERSON_FILE, PERSON_IO.INOUT_FILE, PERSON_FILE_NAME);
    end;
  exception
    when others =>
      raise IO_ERROR;
  end OPEN;

  -- Load the list from the file. (Erasing the current list)
  procedure LOAD is
    PERSON : PERS_DEF.PERSON_REC;
  begin

    -- Open file
    OPEN;
    -- Clear list
    PERS_DEF.PERSON_LIST_MNG.DELETE_LIST (THE_PERSONS);

    -- Read persons from file and insert them in list
    while not PERSON_IO.END_OF_FILE (PERSON_FILE) loop
      PERSON_IO.READ (PERSON_FILE, PERSON);
      PERS_DEF.PERSON_LIST_MNG.INSERT (THE_PERSONS, PERSON,
       PERS_DEF.PERSON_LIST_MNG.NEXT);
    end loop;

    -- Close file
    PERSON_IO.CLOSE (PERSON_FILE);
    -- Move to begining of list
    if not PERS_DEF.PERSON_LIST_MNG.IS_EMPTY(THE_PERSONS) then
      PERS_DEF.PERSON_LIST_MNG.MOVE_TO (THE_PERSONS,
       PERS_DEF.PERSON_LIST_MNG.NEXT, 0, FALSE);
    end if;

  exception
    when PERS_DEF.PERSON_LIST_MNG.FULL_LIST =>
      raise FULL_LIST_ERROR;
    when others =>
      raise IO_ERROR;
  end LOAD;


  -- Save the list to file. (List not affected)
  procedure SAVE is
    PERSON : PERS_DEF.PERSON_REC;
    LIST_LENGTH : constant NATURAL
                := PERS_DEF.PERSON_LIST_MNG.LIST_LENGTH (THE_PERSONS);
    INIT_POS    : NATURAL;
  begin

    -- Delete file and create a new empty one
    OPEN;
    PERSON_IO.DELETE (PERSON_FILE);
    OPEN;

    -- Scan list only if not empty
    if LIST_LENGTH /= 0 then
      -- Save current position
      INIT_POS := PERS_DEF.PERSON_LIST_MNG.GET_POSITION (THE_PERSONS);
      -- Move to beginning of list
      PERS_DEF.PERSON_LIST_MNG.MOVE_TO (THE_PERSONS,
       PERS_DEF.PERSON_LIST_MNG.NEXT, 0, FALSE);

      -- Read persons from list and write them to file
      for I in 1 .. LIST_LENGTH loop
        if I /= LIST_LENGTH then
          PERS_DEF.PERSON_LIST_MNG.READ (THE_PERSONS, PERSON);
        else
          -- Do not move after reading last person
          PERS_DEF.PERSON_LIST_MNG.READ (THE_PERSONS, PERSON,
           PERS_DEF.PERSON_LIST_MNG.CURRENT);
        end if;
        PERSON_IO.WRITE (PERSON_FILE, PERSON);
      end loop;

      -- Move to initial position in list
      PERS_DEF.PERSON_LIST_MNG.MOVE_TO (THE_PERSONS,
       PERS_DEF.PERSON_LIST_MNG.NEXT, INIT_POS - 1, FALSE);
    end if;

    -- Close file
    PERSON_IO.CLOSE (PERSON_FILE);

  exception
    when PERS_DEF.PERSON_LIST_MNG.EMPTY_LIST |
         PERS_DEF.PERSON_LIST_MNG.NOT_IN_LIST =>
      raise PERS_FIL_INTERNAL_ERROR;
    when others =>
      raise IO_ERROR;
  end SAVE;

end PERS_FIL;

