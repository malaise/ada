with AFPX;
with PERS_DEF, MESU_DEF, MESU_NAM;

-- Mesure selection management
package MESU_SEL is


  -- Criteria for adding to / removing from selection
  type CRITERIA_REC is record
    NAME     : PERS_DEF.PERSON_NAME_STR;
    ACTIVITY : PERS_DEF.PERSON_ACTIVITY_STR;
    DATE_AFT : MESU_DEF.DATE_STR;
    DATE_BEF : MESU_DEF.DATE_STR;
  end record;

  -- Add records to selection
  procedure ADD_SELECTION (CRITERIA : in CRITERIA_REC);

  -- Remove records from selection
  procedure REM_SELECTION (CRITERIA : in CRITERIA_REC);

  -- Add a record to selection
  procedure ADD_SELECTION (NAME : in MESU_NAM.FILE_NAME_STR);

  -- Remove a record from selection
  procedure REM_SELECTION (NAME : in MESU_NAM.FILE_NAME_STR);
  -- This one to be used if the record file is already deleted
  procedure REM_SELECTION (LINE : in AFPX.LINE_REC);


  -- Load the selection from file
  procedure LOAD;

  -- Save the selection to file
  procedure SAVE;

  -- Undo (if possible) previous action on selection
  procedure UNDO;

  -- Copy of afpx list
  procedure COPY_LIST (FROM, TO : in out AFPX.LINE_LIST_MNG.LIST_TYPE);

end MESU_SEL;