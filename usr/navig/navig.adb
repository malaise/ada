with NAV_SCREEN, NAV_DATA, NAV_DIALOG;
procedure NAVIG is
  DATA, RESULT : NAV_DATA.T_DATA;
  ACTION : NAV_DIALOG.ACTION;
  STATUS : NAV_DATA.T_CONSISTENCY;
begin
  NAV_DIALOG.INIT;
  loop

    -- get data and the action
    NAV_DIALOG.GET (DATA, ACTION);
    if NAV_SCREEN."=" (ACTION, NAV_SCREEN.QUIT) then
      return;
    end if;

    -- compute
    NAV_DATA.RESOLUTION (DATA, STATUS, RESULT);

    if NAV_DATA."=" (STATUS, NAV_DATA.OK) then
      -- put the result
      NAV_DIALOG.PUT (RESULT);
    else
      -- put consistency error message during a while
      NAV_DIALOG.PUT (STATUS);
    end if;

  end loop;
exception
  when others =>
    NAV_DIALOG.ABORT_CLOCK;
    raise;
end NAVIG;
