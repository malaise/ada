with NAV_DATA, NAV_SCREEN;
-- Management of the dialog with the operator

package NAV_DIALOG is

  procedure INIT;

  -- Action to be managed by the dialog (get -> compute or quit)
  subtype ACTION is NAV_SCREEN.ACTION
   range NAV_SCREEN.COMPUTE .. NAV_SCREEN.QUIT;

  -- get data and then the action
  procedure GET (DATA : in out NAV_DATA.T_DATA; TO_DO : out ACTION);

  -- put the result
  procedure PUT (RESULT : in NAV_DATA.T_DATA);
  -- put consistency error message during a while
  procedure PUT (ERROR : in NAV_DATA.T_CONSISTENCY);

  procedure ABORT_CLOCK renames NAV_SCREEN.ABORT_CLOCK;

end NAV_DIALOG;


