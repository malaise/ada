with SCREEN;
separate (ACCOUNT)
procedure QUIT is
begin
  if MNG.IS_SAVED or else SCREEN.CONFIRM_ACTION(SCREEN.QUIT_UNSAVED) then 
    raise QUIT_PROGRAM;
  end if;
end QUIT;

