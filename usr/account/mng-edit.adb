separate (MNG)

procedure EDIT (OPER : in out OPER_DEF.OPER_REC;
                EDIT_TYPE : in EDIT_LIST;
                VALID : out BOOLEAN) is
begin

  if EDIT_TYPE = CREATE then
    OPER.DATE := OPER_DEF.CURRENT_DATE;
    OPER.AMOUNT := 0.0;
    OPER.STATUS := OPER_DEF.NOT_ENTERED;
    OPER.KIND := OPER_DEF.TRANSFER;
    OPER.REFERENCE := (others => ' ');
    OPER.DESTINATION := (others => ' ');
    OPER.COMMENT := (others => ' ');
  end if;
  -- @@@
  -- Set title
  -- Set fields protections & buttons
  -- Ptg
  SCREEN.ACK_ERROR(SCREEN.NOT_IMPLEMENTED);
  VALID := FALSE;
end EDIT;

