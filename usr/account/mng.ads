with OPER_DEF;
-- Manage the whole acount status
package MNG is
  -- Nb of operations in current account
  MAX_NB_OPER : constant := 9999;
  subtype OPER_NB_RANGE is NATURAL range 0 .. MAX_NB_OPER;
  subtype OPER_RANGE is POSITIVE range 1 .. MAX_NB_OPER;

  -- Update current (selected) operation
  -- Does nothing with 0 (empty list)
  procedure SET_CURRENT (NO : in OPER_NB_RANGE);

  -- Modify the account. Load and clear check it is saved.
  -- Load a file. If file name is "" then ask for it
  procedure LOAD (FILE_NAME : in STRING);
  -- Save current file after confirmation or in rescue file
  procedure SAVE (RESCUE : in BOOLEAN := FALSE);
  -- Clear current account (and file name)
  procedure CLEAR;
  -- Print listing
  procedure PRINT;
  -- Update the displayed amounts of opers, sums
  procedure CHANGE_UNIT;

  -- Modify operations
  procedure UPDATE_STATE;
  procedure ADD_OPER;
  procedure EDIT_OPER;
  procedure VIEW_OPER;
  procedure DEL_OPER;
  procedure GARBAGE_COLLECT;
  procedure SEARCH;
  procedure SHOW_ALL;

  -- Get data
  function IS_SAVED return BOOLEAN;

end MNG;
