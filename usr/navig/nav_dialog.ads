with Nav_Data, Nav_Screen;
-- Management of the dialog with the operator

package Nav_Dialog is

  procedure Init;

  -- Action to be managed by the dialog (get -> compute or quit)
  subtype Action is Nav_Screen.Dialog_Action;

  -- get data and then the action
  procedure Get (Data : in out Nav_Data.T_Data; To_Do : out Action);

  -- put the result
  procedure Put (Result : in Nav_Data.T_Data);
  -- put consistency error message during a while
  procedure Put (Error : in Nav_Data.T_Consistency);

end Nav_Dialog;


