with OPER_DEF, UNIT_FORMAT;
-- Manage the screen fields
package SCREEN is

  -- Allow oper edition buttons (edit/view, delete, clean_up, search)
  --  and Show_all button at next reset, confirm, ack
  -- False by default
  procedure ALLOW_EDIT (ALLOW : in BOOLEAN);
  procedure SUBLIST (ACTIVE : in BOOLEAN);

  -- Set to default mode
  procedure RESET;

  -- Encode header fields
  procedure ENCODE_FILE_NAME (FILE_NAME : in STRING);
  procedure ENCODE_NB_OPER (OPER : in NATURAL; SELECTED : in NATURAL);
  procedure ENCODE_SAVED (SAVED : in BOOLEAN);

  -- Set the "TO FRANCS/EUROS" button according to current unit
  procedure UPDATE_TO_UNIT;

  -- Encore summary
  procedure ENCODE_SUMMARY(REAL_AMOUNT, ACCOUNT_AMOUNT,
                           DEFERED_AMOUNT, MARGIN_AMOUNT :
                                    in OPER_DEF.AMOUNT_RANGE);

  -- Confirm
  type ACTION_LIST is (OVERWRITE_ACCOUNT, OVERWRITE_FILE, QUIT_UNSAVED);
  function CONFIRM_ACTION (ACTION : ACTION_LIST) return BOOLEAN;

  -- Ack an error
  type ERROR_LIST is (FILE_ACCESS, FILE_IO, FILE_NAME_TOO_LONG, ACCOUNT_FULL,
                      NOT_IMPLEMENTED, INTERNAL_ERROR);
  procedure ACK_ERROR (ERROR : in ERROR_LIST);

  -- Ring alarm / question bell
  procedure RING (ALARM : in BOOLEAN);

end SCREEN;

